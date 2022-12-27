{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_compression_control - device extension
--
-- == VK_EXT_image_compression_control
--
-- [__Name String__]
--     @VK_EXT_image_compression_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     339
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_compression_control] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_image_compression_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_image_compression_control.adoc VK_EXT_image_compression_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Graeme Leese, Broadcom
--
--     -   Andrew Garrard, Imagination
--
--     -   Lisa Wu, Arm
--
--     -   Peter Kohaut, Arm
--
-- == Description
--
-- This extension enables fixed-rate image compression and adds the ability
-- to control when this kind of compression can be applied. Many
-- implementations support some form of framebuffer compression. This is
-- typically transparent to applications as lossless compression schemes
-- are used. With fixed-rate compression, the compression is done at a
-- defined bitrate. Such compression algorithms generally produce results
-- that are visually lossless, but the results are typically not bit-exact
-- when compared to a non-compressed result. The implementation may not be
-- able to use the requested compression rate in all cases. This extension
-- adds a query that can be used to determine the compression scheme and
-- rate that was applied to an image.
--
-- == New Commands
--
-- -   'getImageSubresourceLayout2EXT'
--
-- == New Structures
--
-- -   'ImageSubresource2EXT'
--
-- -   'SubresourceLayout2EXT'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2':
--
--     -   'ImageCompressionControlEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2',
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceFormat2KHR',
--     'SubresourceLayout2EXT':
--
--     -   'ImageCompressionPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageCompressionControlFeaturesEXT'
--
-- == New Enums
--
-- -   'ImageCompressionFixedRateFlagBitsEXT'
--
-- -   'ImageCompressionFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ImageCompressionFixedRateFlagsEXT'
--
-- -   'ImageCompressionFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_COMPRESSION_EXHAUSTED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-05-02 (Jan-Harald Fredriksen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'ImageCompressionControlEXT', 'ImageCompressionFixedRateFlagBitsEXT',
-- 'ImageCompressionFixedRateFlagsEXT', 'ImageCompressionFlagBitsEXT',
-- 'ImageCompressionFlagsEXT', 'ImageCompressionPropertiesEXT',
-- 'ImageSubresource2EXT',
-- 'PhysicalDeviceImageCompressionControlFeaturesEXT',
-- 'SubresourceLayout2EXT', 'getImageSubresourceLayout2EXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_compression_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_compression_control  ( getImageSubresourceLayout2EXT
                                                           , ImageCompressionControlEXT(..)
                                                           , PhysicalDeviceImageCompressionControlFeaturesEXT(..)
                                                           , ImageCompressionPropertiesEXT(..)
                                                           , ImageSubresource2EXT(..)
                                                           , SubresourceLayout2EXT(..)
                                                           , ImageCompressionFlagsEXT
                                                           , ImageCompressionFlagBitsEXT( IMAGE_COMPRESSION_DEFAULT_EXT
                                                                                        , IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT
                                                                                        , IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT
                                                                                        , IMAGE_COMPRESSION_DISABLED_EXT
                                                                                        , ..
                                                                                        )
                                                           , ImageCompressionFixedRateFlagsEXT
                                                           , ImageCompressionFixedRateFlagBitsEXT( IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_1BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_3BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_5BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_6BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_7BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_8BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_9BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_10BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_11BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_12BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_13BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_14BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_15BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_16BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_17BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_18BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_19BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_20BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_21BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_22BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_23BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_24BPC_BIT_EXT
                                                                                                 , ..
                                                                                                 )
                                                           , EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION
                                                           , pattern EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION
                                                           , EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME
                                                           , pattern EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME
                                                           ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Bits (Bits)
import GHC.Bits (FiniteBits)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSubresourceLayout2EXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.SparseResourceMemoryManagement (ImageSubresource)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Image (SubresourceLayout)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSubresourceLayout2EXT
  :: FunPtr (Ptr Device_T -> Image -> Ptr ImageSubresource2EXT -> Ptr (SomeStruct SubresourceLayout2EXT) -> IO ()) -> Ptr Device_T -> Image -> Ptr ImageSubresource2EXT -> Ptr (SomeStruct SubresourceLayout2EXT) -> IO ()

-- | vkGetImageSubresourceLayout2EXT - Retrieve information about an image
-- subresource
--
-- = Description
--
-- 'getImageSubresourceLayout2EXT' behaves similarly to
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', with the ability to
-- specify extended inputs via chained input structures, and to return
-- extended information via chained output structures.
--
-- == Valid Usage
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-image-02270# @image@ /must/
--     have been created with @tiling@ equal to
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-aspectMask-00997# The
--     @aspectMask@ member of @pSubresource@ /must/ only have a single bit
--     set
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-mipLevel-01716# The @mipLevel@
--     member of @pSubresource@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-arrayLayer-01717# The
--     @arrayLayer@ member of @pSubresource@ /must/ be less than the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @image@ was created
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-format-04461# If @format@ is a
--     color format, the @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-format-04462# If @format@ has
--     a depth component, the @aspectMask@ member of @pSubresource@ /must/
--     contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-format-04463# If @format@ has
--     a stencil component, the @aspectMask@ member of @pSubresource@
--     /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-format-04464# If @format@ does
--     not contain a stencil or depth component, the @aspectMask@ member of
--     @pSubresource@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-format-01581# If the @tiling@
--     of the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and its
--     @format@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--     with two planes, the @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-format-01582# If the @tiling@
--     of the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and its
--     @format@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--     with three planes, the @aspectMask@ member of @pSubresource@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-image-01895# If @image@ was
--     created with the
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     external memory handle type, then @image@ /must/ be bound to memory
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-tiling-02271# If the @tiling@
--     of the @image@ is
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
-- -   #VUID-vkGetImageSubresourceLayout2EXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-image-parameter# @image@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-pSubresource-parameter#
--     @pSubresource@ /must/ be a valid pointer to a valid
--     'ImageSubresource2EXT' structure
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-pLayout-parameter# @pLayout@
--     /must/ be a valid pointer to a 'SubresourceLayout2EXT' structure
--
-- -   #VUID-vkGetImageSubresourceLayout2EXT-image-parent# @image@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'ImageSubresource2EXT', 'SubresourceLayout2EXT'
getImageSubresourceLayout2EXT :: forall a io
                               . ( Extendss SubresourceLayout2EXT a
                                 , PokeChain a
                                 , PeekChain a
                                 , MonadIO io )
                              => -- | @device@ is the logical device that owns the image.
                                 Device
                              -> -- | @image@ is the image whose layout is being queried.
                                 Image
                              -> -- | @pSubresource@ is a pointer to a 'ImageSubresource2EXT' structure
                                 -- selecting a specific image for the image subresource.
                                 ImageSubresource2EXT
                              -> io (SubresourceLayout2EXT a)
getImageSubresourceLayout2EXT device image subresource = liftIO . evalContT $ do
  let vkGetImageSubresourceLayout2EXTPtr = pVkGetImageSubresourceLayout2EXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageSubresourceLayout2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSubresourceLayout2EXT is null" Nothing Nothing
  let vkGetImageSubresourceLayout2EXT' = mkVkGetImageSubresourceLayout2EXT vkGetImageSubresourceLayout2EXTPtr
  pSubresource <- ContT $ withCStruct (subresource)
  pPLayout <- ContT (withZeroCStruct @(SubresourceLayout2EXT _))
  lift $ traceAroundEvent "vkGetImageSubresourceLayout2EXT" (vkGetImageSubresourceLayout2EXT'
                                                               (deviceHandle (device))
                                                               (image)
                                                               pSubresource
                                                               (forgetExtensions (pPLayout)))
  pLayout <- lift $ peekCStruct @(SubresourceLayout2EXT _) pPLayout
  pure $ (pLayout)


-- | VkImageCompressionControlEXT - Specify image compression properties
--
-- = Description
--
-- If enabled, fixed-rate compression is done in an implementation-defined
-- manner and /may/ be applied at block granularity. In that case, a write
-- to an individual texel /may/ modify the value of other texels in the
-- same block.
--
-- == Valid Usage
--
-- -   #VUID-VkImageCompressionControlEXT-flags-06747# @flags@ /must/ be
--     one of 'IMAGE_COMPRESSION_DEFAULT_EXT',
--     'IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT',
--     'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT', or
--     'IMAGE_COMPRESSION_DISABLED_EXT'
--
-- -   #VUID-VkImageCompressionControlEXT-flags-06748# If @flags@ includes
--     'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT', @pFixedRateFlags@
--     /must/ not be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageCompressionControlEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT'
--
-- Note
--
-- Some combinations of compression properties may not be supported. For
-- example, some implementations may not support different fixed-rate
-- compression rates per plane of a multi-planar format and will not be
-- able to enable fixed-rate compression for any plane if the requested
-- rates differ.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'ImageCompressionFixedRateFlagsEXT', 'ImageCompressionFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageCompressionControlEXT = ImageCompressionControlEXT
  { -- | @flags@ is a bitmask of 'ImageCompressionFlagBitsEXT' describing
    -- compression controls for the image.
    flags :: ImageCompressionFlagsEXT
  , -- | @compressionControlPlaneCount@ is the number of entries in the
    -- @pFixedRateFlags@ array.
    compressionControlPlaneCount :: Word32
  , -- | @pFixedRateFlags@ is @NULL@ or a pointer to an array of
    -- 'ImageCompressionFixedRateFlagsEXT' bitfields describing allowed
    -- fixed-rate compression rates of each image plane. It is ignored if
    -- @flags@ does not include 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT'.
    fixedRateFlags :: Ptr ImageCompressionFixedRateFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCompressionControlEXT)
#endif
deriving instance Show ImageCompressionControlEXT

instance ToCStruct ImageCompressionControlEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCompressionControlEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (compressionControlPlaneCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ImageCompressionFixedRateFlagsEXT))) (fixedRateFlags)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ImageCompressionFixedRateFlagsEXT))) (zero)
    f

instance FromCStruct ImageCompressionControlEXT where
  peekCStruct p = do
    flags <- peek @ImageCompressionFlagsEXT ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT))
    compressionControlPlaneCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pFixedRateFlags <- peek @(Ptr ImageCompressionFixedRateFlagsEXT) ((p `plusPtr` 24 :: Ptr (Ptr ImageCompressionFixedRateFlagsEXT)))
    pure $ ImageCompressionControlEXT
             flags compressionControlPlaneCount pFixedRateFlags

instance Storable ImageCompressionControlEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCompressionControlEXT where
  zero = ImageCompressionControlEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceImageCompressionControlFeaturesEXT - Structure
-- describing whether image compression controls can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceImageCompressionControlFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageCompressionControlFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageCompressionControlFeaturesEXT = PhysicalDeviceImageCompressionControlFeaturesEXT
  { -- | #features-imageCompressionControl# @imageCompressionControl@ indicates
    -- that the implementation supports providing controls for image
    -- compression at image creation time.
    imageCompressionControl :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageCompressionControlFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceImageCompressionControlFeaturesEXT

instance ToCStruct PhysicalDeviceImageCompressionControlFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageCompressionControlFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imageCompressionControl))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageCompressionControlFeaturesEXT where
  peekCStruct p = do
    imageCompressionControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageCompressionControlFeaturesEXT
             (bool32ToBool imageCompressionControl)

instance Storable PhysicalDeviceImageCompressionControlFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageCompressionControlFeaturesEXT where
  zero = PhysicalDeviceImageCompressionControlFeaturesEXT
           zero


-- | VkImageCompressionPropertiesEXT - Compression properties of an image
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'ImageCompressionFixedRateFlagsEXT', 'ImageCompressionFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageCompressionPropertiesEXT = ImageCompressionPropertiesEXT
  { -- | @imageCompressionFlags@ returns a value describing the compression
    -- controls that apply to the image. The value will be either
    -- 'IMAGE_COMPRESSION_DEFAULT_EXT' to indicate no fixed-rate compression,
    -- 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT' to indicate fixed-rate
    -- compression, or 'IMAGE_COMPRESSION_DISABLED_EXT' to indicate no
    -- compression.
    imageCompressionFlags :: ImageCompressionFlagsEXT
  , -- | @imageCompressionFixedRateFlags@ returns a
    -- 'ImageCompressionFixedRateFlagsEXT' value describing the compression
    -- rates that apply to the specified aspect of the image.
    imageCompressionFixedRateFlags :: ImageCompressionFixedRateFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCompressionPropertiesEXT)
#endif
deriving instance Show ImageCompressionPropertiesEXT

instance ToCStruct ImageCompressionPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCompressionPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT)) (imageCompressionFlags)
    poke ((p `plusPtr` 20 :: Ptr ImageCompressionFixedRateFlagsEXT)) (imageCompressionFixedRateFlags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ImageCompressionFixedRateFlagsEXT)) (zero)
    f

instance FromCStruct ImageCompressionPropertiesEXT where
  peekCStruct p = do
    imageCompressionFlags <- peek @ImageCompressionFlagsEXT ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT))
    imageCompressionFixedRateFlags <- peek @ImageCompressionFixedRateFlagsEXT ((p `plusPtr` 20 :: Ptr ImageCompressionFixedRateFlagsEXT))
    pure $ ImageCompressionPropertiesEXT
             imageCompressionFlags imageCompressionFixedRateFlags

instance Storable ImageCompressionPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCompressionPropertiesEXT where
  zero = ImageCompressionPropertiesEXT
           zero
           zero


-- | VkImageSubresource2EXT - Structure specifying an image subresource
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageSubresourceLayout2EXT'
data ImageSubresource2EXT = ImageSubresource2EXT
  { -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource'
    -- structure.
    --
    -- #VUID-VkImageSubresource2EXT-imageSubresource-parameter#
    -- @imageSubresource@ /must/ be a valid
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource'
    -- structure
    imageSubresource :: ImageSubresource }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresource2EXT)
#endif
deriving instance Show ImageSubresource2EXT

instance ToCStruct ImageSubresource2EXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresource2EXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresource)) (imageSubresource)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresource)) (zero)
    f

instance FromCStruct ImageSubresource2EXT where
  peekCStruct p = do
    imageSubresource <- peekCStruct @ImageSubresource ((p `plusPtr` 16 :: Ptr ImageSubresource))
    pure $ ImageSubresource2EXT
             imageSubresource

instance Storable ImageSubresource2EXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresource2EXT where
  zero = ImageSubresource2EXT
           zero


-- | VkSubresourceLayout2EXT - Structure specifying subresource layout
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubresourceLayout2EXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT'
--
-- -   #VUID-VkSubresourceLayout2EXT-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of 'ImageCompressionPropertiesEXT'
--
-- -   #VUID-VkSubresourceLayout2EXT-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Image.SubresourceLayout', 'getImageSubresourceLayout2EXT'
data SubresourceLayout2EXT (es :: [Type]) = SubresourceLayout2EXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @subresourceLayout@ is a 'Vulkan.Core10.Image.SubresourceLayout'
    -- structure.
    subresourceLayout :: SubresourceLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubresourceLayout2EXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubresourceLayout2EXT es)

instance Extensible SubresourceLayout2EXT where
  extensibleTypeName = "SubresourceLayout2EXT"
  setNext SubresourceLayout2EXT{..} next' = SubresourceLayout2EXT{next = next', ..}
  getNext SubresourceLayout2EXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubresourceLayout2EXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageCompressionPropertiesEXT = Just f
    | otherwise = Nothing

instance ( Extendss SubresourceLayout2EXT es
         , PokeChain es ) => ToCStruct (SubresourceLayout2EXT es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubresourceLayout2EXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SubresourceLayout)) (subresourceLayout)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr SubresourceLayout)) (zero)
    lift $ f

instance ( Extendss SubresourceLayout2EXT es
         , PeekChain es ) => FromCStruct (SubresourceLayout2EXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    subresourceLayout <- peekCStruct @SubresourceLayout ((p `plusPtr` 16 :: Ptr SubresourceLayout))
    pure $ SubresourceLayout2EXT
             next subresourceLayout

instance es ~ '[] => Zero (SubresourceLayout2EXT es) where
  zero = SubresourceLayout2EXT
           ()
           zero


type ImageCompressionFlagsEXT = ImageCompressionFlagBitsEXT

-- | VkImageCompressionFlagBitsEXT - Bitmask specifying image compression
-- controls
--
-- = Description
--
-- If 'ImageCompressionControlEXT'::@flags@ is
-- 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT', then the @i@th member of
-- the @pFixedRateFlags@ array specifies the allowed compression rates for
-- the image’s @i@th plane.
--
-- Note
--
-- If 'IMAGE_COMPRESSION_DISABLED_EXT' is included in
-- 'ImageCompressionControlEXT'::@flags@, both lossless and fixed-rate
-- compression will be disabled. This is likely to have a negative impact
-- on performance and is only intended to be used for debugging purposes.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'ImageCompressionFlagsEXT'
newtype ImageCompressionFlagBitsEXT = ImageCompressionFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_COMPRESSION_DEFAULT_EXT' specifies that the default image
-- compression setting is used. Implementations /must/ not apply fixed-rate
-- compression.
pattern IMAGE_COMPRESSION_DEFAULT_EXT = ImageCompressionFlagBitsEXT 0x00000000

-- | 'IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT' specifies that the
-- implementation /may/ choose any supported fixed-rate compression setting
-- in an implementation-defined manner based on the properties of the
-- image.
pattern IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT = ImageCompressionFlagBitsEXT 0x00000001

-- | 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT' specifies that fixed-rate
-- compression /may/ be used and that the allowed compression rates are
-- specified by 'ImageCompressionControlEXT'::@pFixedRateFlags@.
pattern IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT = ImageCompressionFlagBitsEXT 0x00000002

-- | 'IMAGE_COMPRESSION_DISABLED_EXT' specifies that all lossless and
-- fixed-rate compression /should/ be disabled.
pattern IMAGE_COMPRESSION_DISABLED_EXT = ImageCompressionFlagBitsEXT 0x00000004

conNameImageCompressionFlagBitsEXT :: String
conNameImageCompressionFlagBitsEXT = "ImageCompressionFlagBitsEXT"

enumPrefixImageCompressionFlagBitsEXT :: String
enumPrefixImageCompressionFlagBitsEXT = "IMAGE_COMPRESSION_"

showTableImageCompressionFlagBitsEXT :: [(ImageCompressionFlagBitsEXT, String)]
showTableImageCompressionFlagBitsEXT =
  [
    ( IMAGE_COMPRESSION_DEFAULT_EXT
    , "DEFAULT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT
    , "FIXED_RATE_DEFAULT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT
    , "FIXED_RATE_EXPLICIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_DISABLED_EXT
    , "DISABLED_EXT"
    )
  ]

instance Show ImageCompressionFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixImageCompressionFlagBitsEXT
      showTableImageCompressionFlagBitsEXT
      conNameImageCompressionFlagBitsEXT
      (\(ImageCompressionFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ImageCompressionFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixImageCompressionFlagBitsEXT
      showTableImageCompressionFlagBitsEXT
      conNameImageCompressionFlagBitsEXT
      ImageCompressionFlagBitsEXT

type ImageCompressionFixedRateFlagsEXT = ImageCompressionFixedRateFlagBitsEXT

-- | VkImageCompressionFixedRateFlagBitsEXT - Bitmask specifying fixed rate
-- image compression rates
--
-- = Description
--
-- If the format has a different bit rate for different components,
-- 'ImageCompressionControlEXT'::@pFixedRateFlags@ describes the rate of
-- the component with the largest number of bits assigned to it, scaled pro
-- rata. For example, to request that a
-- 'Vulkan.Core10.Enums.Format.FORMAT_A2R10G10B10_UNORM_PACK32' format be
-- stored at a rate of 8 bits per pixel, use
-- 'IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT' (10 bits for the largest
-- component, stored at quarter the original size, 2.5 bits, rounded down).
--
-- If @flags@ includes 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT', and
-- multiple bits are set in 'ImageCompressionControlEXT'::@pFixedRateFlags@
-- for a plane, implementations /should/ apply the lowest allowed bitrate
-- that is supported.
--
-- Note
--
-- The choice of “bits per component” terminology was chosen so that the
-- same compression rate describes the same degree of compression applied
-- to formats that differ only in the number of components. For example,
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8_UNORM' compressed to half its
-- original size is a rate of 4 bits per component, 8 bits per pixel.
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM' compressed to half
-- /its/ original size is 4 bits per component, 16 bits per pixel. Both of
-- these cases can be requested with
-- 'IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'ImageCompressionFixedRateFlagsEXT'
newtype ImageCompressionFixedRateFlagBitsEXT = ImageCompressionFixedRateFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT' specifies that fixed-rate
-- compression /must/ not be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000000

-- | 'IMAGE_COMPRESSION_FIXED_RATE_1BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [1,2) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_1BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000001

-- | 'IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [2,3) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000002

-- | 'IMAGE_COMPRESSION_FIXED_RATE_3BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [3,4) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_3BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000004

-- | 'IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [4,5) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000008

-- | 'IMAGE_COMPRESSION_FIXED_RATE_5BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [5,6) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_5BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000010

-- | 'IMAGE_COMPRESSION_FIXED_RATE_6BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [6,7) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_6BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000020

-- | 'IMAGE_COMPRESSION_FIXED_RATE_7BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [7,8) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_7BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000040

-- | 'IMAGE_COMPRESSION_FIXED_RATE_8BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [8,9) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_8BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000080

-- | 'IMAGE_COMPRESSION_FIXED_RATE_9BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [9,10) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_9BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000100

-- | 'IMAGE_COMPRESSION_FIXED_RATE_10BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [10,11) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_10BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000200

-- | 'IMAGE_COMPRESSION_FIXED_RATE_11BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [11,12) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_11BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000400

-- | 'IMAGE_COMPRESSION_FIXED_RATE_12BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of at least 12 bits per component /may/ be
-- used.
pattern IMAGE_COMPRESSION_FIXED_RATE_12BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000800

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_13BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_13BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00001000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_14BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_14BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00002000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_15BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_15BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00004000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_16BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_16BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00008000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_17BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_17BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00010000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_18BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_18BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00020000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_19BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_19BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00040000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_20BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_20BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00080000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_21BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_21BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00100000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_22BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_22BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00200000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_23BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_23BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00400000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_24BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_24BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00800000

conNameImageCompressionFixedRateFlagBitsEXT :: String
conNameImageCompressionFixedRateFlagBitsEXT = "ImageCompressionFixedRateFlagBitsEXT"

enumPrefixImageCompressionFixedRateFlagBitsEXT :: String
enumPrefixImageCompressionFixedRateFlagBitsEXT = "IMAGE_COMPRESSION_FIXED_RATE_"

showTableImageCompressionFixedRateFlagBitsEXT :: [(ImageCompressionFixedRateFlagBitsEXT, String)]
showTableImageCompressionFixedRateFlagBitsEXT =
  [
    ( IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT
    , "NONE_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_1BPC_BIT_EXT
    , "1BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT
    , "2BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_3BPC_BIT_EXT
    , "3BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT
    , "4BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_5BPC_BIT_EXT
    , "5BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_6BPC_BIT_EXT
    , "6BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_7BPC_BIT_EXT
    , "7BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_8BPC_BIT_EXT
    , "8BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_9BPC_BIT_EXT
    , "9BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_10BPC_BIT_EXT
    , "10BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_11BPC_BIT_EXT
    , "11BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_12BPC_BIT_EXT
    , "12BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_13BPC_BIT_EXT
    , "13BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_14BPC_BIT_EXT
    , "14BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_15BPC_BIT_EXT
    , "15BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_16BPC_BIT_EXT
    , "16BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_17BPC_BIT_EXT
    , "17BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_18BPC_BIT_EXT
    , "18BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_19BPC_BIT_EXT
    , "19BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_20BPC_BIT_EXT
    , "20BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_21BPC_BIT_EXT
    , "21BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_22BPC_BIT_EXT
    , "22BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_23BPC_BIT_EXT
    , "23BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_24BPC_BIT_EXT
    , "24BPC_BIT_EXT"
    )
  ]

instance Show ImageCompressionFixedRateFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixImageCompressionFixedRateFlagBitsEXT
      showTableImageCompressionFixedRateFlagBitsEXT
      conNameImageCompressionFixedRateFlagBitsEXT
      (\(ImageCompressionFixedRateFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ImageCompressionFixedRateFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixImageCompressionFixedRateFlagBitsEXT
      showTableImageCompressionFixedRateFlagBitsEXT
      conNameImageCompressionFixedRateFlagBitsEXT
      ImageCompressionFixedRateFlagBitsEXT

type EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION"
pattern EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION = 1


type EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME = "VK_EXT_image_compression_control"

-- No documentation found for TopLevel "VK_EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME"
pattern EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME = "VK_EXT_image_compression_control"

