{-# language CPP #-}
-- | = Name
--
-- VK_EXT_host_image_copy - device extension
--
-- == VK_EXT_host_image_copy
--
-- [__Name String__]
--     @VK_EXT_host_image_copy@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     271
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_host_image_copy] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_host_image_copy extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_host_image_copy.adoc VK_EXT_host_image_copy>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-04-26
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Faith Ekstrand, Collabora
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   James Fitzpatrick, Imagination
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- This extension allows applications to copy data between host memory and
-- images on the host processor, without staging the data through a
-- GPU-accessible buffer. This removes the need to allocate and manage the
-- buffer and its associated memory. On some architectures it may also
-- eliminate an extra copy operation. This extension additionally allows
-- applications to copy data between images on the host.
--
-- To support initializing a new image in preparation for a host copy, it
-- is now possible to transition a new image to
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or other
-- host-copyable layouts via 'transitionImageLayoutEXT'. Additionally, it
-- is possible to perform copies that preserve the swizzling layout of the
-- image by using the 'HOST_IMAGE_COPY_MEMCPY_EXT' flag. In that case, the
-- memory size needed for copies to or from a buffer can be retrieved by
-- chaining 'SubresourceHostMemcpySizeEXT' to @pLayout@ in
-- 'getImageSubresourceLayout2EXT'.
--
-- == New Commands
--
-- -   'copyImageToImageEXT'
--
-- -   'copyImageToMemoryEXT'
--
-- -   'copyMemoryToImageEXT'
--
-- -   'getImageSubresourceLayout2EXT'
--
-- -   'transitionImageLayoutEXT'
--
-- == New Structures
--
-- -   'CopyImageToImageInfoEXT'
--
-- -   'CopyImageToMemoryInfoEXT'
--
-- -   'CopyMemoryToImageInfoEXT'
--
-- -   'HostImageLayoutTransitionInfoEXT'
--
-- -   'ImageSubresource2EXT'
--
-- -   'ImageToMemoryCopyEXT'
--
-- -   'MemoryToImageCopyEXT'
--
-- -   'SubresourceLayout2EXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2':
--
--     -   'HostImageCopyDevicePerformanceQueryEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceHostImageCopyFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceHostImageCopyPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_maintenance5.SubresourceLayout2KHR':
--
--     -   'SubresourceHostMemcpySizeEXT'
--
-- == New Enums
--
-- -   'HostImageCopyFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'HostImageCopyFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_HOST_IMAGE_COPY_EXTENSION_NAME'
--
-- -   'EXT_HOST_IMAGE_COPY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT'
--
-- == Issues
--
-- 1) When uploading data to an image, the data is usually loaded from
-- disk. Why not have the application load the data directly into a
-- 'Vulkan.Core10.Handles.DeviceMemory' bound to a buffer (instead of host
-- memory), and use
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage'? The same
-- could be done when downloading data from an image.
--
-- __RESOLVED__: This may not always be possible. Complicated Vulkan
-- applications such as game engines often have decoupled subsystems for
-- streaming data and rendering. It may be unreasonable to require the
-- streaming subsystem to coordinate with the rendering subsystem to
-- allocate memory on its behalf, especially as Vulkan may not be the only
-- API supported by the engine. In emulation layers, the image data is
-- necessarily provided by the application in host memory, so an
-- optimization as suggested is not possible. Most importantly, the device
-- memory may not be mappable by an application, but still accessible to
-- the driver.
--
-- 2) Are @optimalBufferCopyOffsetAlignment@ and
-- @optimalBufferCopyRowPitchAlignment@ applicable to host memory as well
-- with the functions introduced by this extension? Or should there be new
-- limits?
--
-- __RESOLVED__: No alignment requirements for the host memory pointer.
--
-- 3) Should there be granularity requirements for image offsets and
-- extents?
--
-- __RESOLVED__: No granularity requirements, i.e. a granularity of 1 pixel
-- (for non-compressed formats) and 1 texel block (for compressed formats)
-- is assumed.
--
-- 4) How should the application deal with layout transitions before or
-- after copying to or from images?
--
-- __RESOLVED__: An existing issue with linear images is that when
-- emulating other APIs, it is impossible to know when to transition them
-- as they are written to by the host and then used bindlessly. The copy
-- operations in this extension are affected by the same limitation. A new
-- command is thus introduced by this extension to address this problem by
-- allowing the host to perform an image layout transition between a
-- handful of layouts.
--
-- == Version History
--
-- -   Revision 0, 2021-01-20 (Faith Ekstrand)
--
--     -   Initial idea and xml
--
-- -   Revision 1, 2023-04-26 (Shahbaz Youssefi)
--
--     -   Initial revision
--
-- == See Also
--
-- 'CopyImageToImageInfoEXT', 'CopyImageToMemoryInfoEXT',
-- 'CopyMemoryToImageInfoEXT', 'HostImageCopyDevicePerformanceQueryEXT',
-- 'HostImageCopyFlagBitsEXT', 'HostImageCopyFlagsEXT',
-- 'HostImageLayoutTransitionInfoEXT', 'ImageSubresource2EXT',
-- 'ImageToMemoryCopyEXT', 'MemoryToImageCopyEXT',
-- 'PhysicalDeviceHostImageCopyFeaturesEXT',
-- 'PhysicalDeviceHostImageCopyPropertiesEXT',
-- 'SubresourceHostMemcpySizeEXT', 'SubresourceLayout2EXT',
-- 'copyImageToImageEXT', 'copyImageToMemoryEXT', 'copyMemoryToImageEXT',
-- 'getImageSubresourceLayout2EXT', 'transitionImageLayoutEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_host_image_copy Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_host_image_copy  ( copyMemoryToImageEXT
                                                 , copyImageToMemoryEXT
                                                 , copyImageToImageEXT
                                                 , transitionImageLayoutEXT
                                                 , getImageSubresourceLayout2EXT
                                                 , PhysicalDeviceHostImageCopyFeaturesEXT(..)
                                                 , PhysicalDeviceHostImageCopyPropertiesEXT(..)
                                                 , MemoryToImageCopyEXT(..)
                                                 , ImageToMemoryCopyEXT(..)
                                                 , CopyMemoryToImageInfoEXT(..)
                                                 , CopyImageToMemoryInfoEXT(..)
                                                 , CopyImageToImageInfoEXT(..)
                                                 , HostImageLayoutTransitionInfoEXT(..)
                                                 , SubresourceHostMemcpySizeEXT(..)
                                                 , HostImageCopyDevicePerformanceQueryEXT(..)
                                                 , HostImageCopyFlagsEXT
                                                 , HostImageCopyFlagBitsEXT( HOST_IMAGE_COPY_MEMCPY_EXT
                                                                           , ..
                                                                           )
                                                 , ImageSubresource2EXT
                                                 , SubresourceLayout2EXT
                                                 , EXT_HOST_IMAGE_COPY_SPEC_VERSION
                                                 , pattern EXT_HOST_IMAGE_COPY_SPEC_VERSION
                                                 , EXT_HOST_IMAGE_COPY_EXTENSION_NAME
                                                 , pattern EXT_HOST_IMAGE_COPY_EXTENSION_NAME
                                                 , ImageSubresource2KHR(..)
                                                 , SubresourceLayout2KHR(..)
                                                 , getImageSubresourceLayout2KHR
                                                 ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Extensions.VK_KHR_maintenance5 (getImageSubresourceLayout2KHR)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCopyImageToImageEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCopyImageToMemoryEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCopyMemoryToImageEXT))
import Vulkan.Dynamic (DeviceCmds(pVkTransitionImageLayoutEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ImageCopy2)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Extensions.VK_KHR_maintenance5 (ImageSubresource2KHR)
import Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import Vulkan.Core10.ImageView (ImageSubresourceRange)
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_KHR_maintenance5 (SubresourceLayout2KHR)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_maintenance5 (getImageSubresourceLayout2KHR)
import Vulkan.Extensions.VK_KHR_maintenance5 (ImageSubresource2KHR(..))
import Vulkan.Extensions.VK_KHR_maintenance5 (SubresourceLayout2KHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyMemoryToImageEXT
  :: FunPtr (Ptr Device_T -> Ptr CopyMemoryToImageInfoEXT -> IO Result) -> Ptr Device_T -> Ptr CopyMemoryToImageInfoEXT -> IO Result

-- | vkCopyMemoryToImageEXT - Copy data from host memory into an image
--
-- = Description
--
-- This command is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdCopyBufferToImage2',
-- except it is executed on the host and reads from host memory instead of
-- a buffer.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyMemoryToImageEXT-hostImageCopy-09058# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-hostImageCopy hostImageCopy>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyMemoryToImageEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyMemoryToImageEXT-pCopyMemoryToImageInfo-parameter#
--     @pCopyMemoryToImageInfo@ /must/ be a valid pointer to a valid
--     'CopyMemoryToImageInfoEXT' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'CopyMemoryToImageInfoEXT', 'Vulkan.Core10.Handles.Device'
copyMemoryToImageEXT :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the device which owns @pCopyMemoryToImageInfo->dstImage@.
                        Device
                     -> -- | @pCopyMemoryToImageInfo@ is a pointer to a 'CopyMemoryToImageInfoEXT'
                        -- structure describing the copy parameters.
                        CopyMemoryToImageInfoEXT
                     -> io ()
copyMemoryToImageEXT device copyMemoryToImageInfo = liftIO . evalContT $ do
  let vkCopyMemoryToImageEXTPtr = pVkCopyMemoryToImageEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyMemoryToImageEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyMemoryToImageEXT is null" Nothing Nothing
  let vkCopyMemoryToImageEXT' = mkVkCopyMemoryToImageEXT vkCopyMemoryToImageEXTPtr
  pCopyMemoryToImageInfo <- ContT $ withCStruct (copyMemoryToImageInfo)
  r <- lift $ traceAroundEvent "vkCopyMemoryToImageEXT" (vkCopyMemoryToImageEXT'
                                                           (deviceHandle (device))
                                                           pCopyMemoryToImageInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyImageToMemoryEXT
  :: FunPtr (Ptr Device_T -> Ptr CopyImageToMemoryInfoEXT -> IO Result) -> Ptr Device_T -> Ptr CopyImageToMemoryInfoEXT -> IO Result

-- | vkCopyImageToMemoryEXT - Copy image data into host memory
--
-- = Description
--
-- This command is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdCopyImageToBuffer2',
-- except it is executed on the host and writes to host memory instead of a
-- buffer.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyImageToMemoryEXT-hostImageCopy-09063# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-hostImageCopy hostImageCopy>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyImageToMemoryEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyImageToMemoryEXT-pCopyImageToMemoryInfo-parameter#
--     @pCopyImageToMemoryInfo@ /must/ be a valid pointer to a valid
--     'CopyImageToMemoryInfoEXT' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'CopyImageToMemoryInfoEXT', 'Vulkan.Core10.Handles.Device'
copyImageToMemoryEXT :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the device which owns @pCopyImageToMemoryInfo->srcImage@.
                        Device
                     -> -- | @pCopyImageToMemoryInfo@ is a pointer to a 'CopyImageToMemoryInfoEXT'
                        -- structure describing the copy parameters.
                        CopyImageToMemoryInfoEXT
                     -> io ()
copyImageToMemoryEXT device copyImageToMemoryInfo = liftIO . evalContT $ do
  let vkCopyImageToMemoryEXTPtr = pVkCopyImageToMemoryEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyImageToMemoryEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyImageToMemoryEXT is null" Nothing Nothing
  let vkCopyImageToMemoryEXT' = mkVkCopyImageToMemoryEXT vkCopyImageToMemoryEXTPtr
  pCopyImageToMemoryInfo <- ContT $ withCStruct (copyImageToMemoryInfo)
  r <- lift $ traceAroundEvent "vkCopyImageToMemoryEXT" (vkCopyImageToMemoryEXT'
                                                           (deviceHandle (device))
                                                           pCopyImageToMemoryInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyImageToImageEXT
  :: FunPtr (Ptr Device_T -> Ptr CopyImageToImageInfoEXT -> IO Result) -> Ptr Device_T -> Ptr CopyImageToImageInfoEXT -> IO Result

-- | vkCopyImageToImageEXT - Copy image data using the host
--
-- = Description
--
-- This command is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdCopyImage2',
-- except it is executed on the host.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyImageToImageEXT-hostImageCopy-09068# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-hostImageCopy hostImageCopy>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyImageToImageEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyImageToImageEXT-pCopyImageToImageInfo-parameter#
--     @pCopyImageToImageInfo@ /must/ be a valid pointer to a valid
--     'CopyImageToImageInfoEXT' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'CopyImageToImageInfoEXT', 'Vulkan.Core10.Handles.Device'
copyImageToImageEXT :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the device which owns @pCopyImageToMemoryInfo->srcImage@.
                       Device
                    -> -- | @pCopyImageToImageInfo@ is a pointer to a 'CopyImageToImageInfoEXT'
                       -- structure describing the copy parameters.
                       CopyImageToImageInfoEXT
                    -> io ()
copyImageToImageEXT device copyImageToImageInfo = liftIO . evalContT $ do
  let vkCopyImageToImageEXTPtr = pVkCopyImageToImageEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyImageToImageEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyImageToImageEXT is null" Nothing Nothing
  let vkCopyImageToImageEXT' = mkVkCopyImageToImageEXT vkCopyImageToImageEXTPtr
  pCopyImageToImageInfo <- ContT $ withCStruct (copyImageToImageInfo)
  r <- lift $ traceAroundEvent "vkCopyImageToImageEXT" (vkCopyImageToImageEXT'
                                                          (deviceHandle (device))
                                                          pCopyImageToImageInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkTransitionImageLayoutEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr HostImageLayoutTransitionInfoEXT -> IO Result) -> Ptr Device_T -> Word32 -> Ptr HostImageLayoutTransitionInfoEXT -> IO Result

-- | vkTransitionImageLayoutEXT - Perform an image layout transition on the
-- host
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'Vulkan.Core10.Handles.Device', 'HostImageLayoutTransitionInfoEXT'
transitionImageLayoutEXT :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the device which owns @pTransitions@[i].@image@.
                            --
                            -- #VUID-vkTransitionImageLayoutEXT-device-parameter# @device@ /must/ be a
                            -- valid 'Vulkan.Core10.Handles.Device' handle
                            Device
                         -> -- | @pTransitions@ is a pointer to an array of
                            -- 'HostImageLayoutTransitionInfoEXT' structures specifying the image and
                            -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views subresource ranges>
                            -- within them to transition.
                            --
                            -- #VUID-vkTransitionImageLayoutEXT-pTransitions-parameter# @pTransitions@
                            -- /must/ be a valid pointer to an array of @transitionCount@ valid
                            -- 'HostImageLayoutTransitionInfoEXT' structures
                            ("transitions" ::: Vector HostImageLayoutTransitionInfoEXT)
                         -> io ()
transitionImageLayoutEXT device transitions = liftIO . evalContT $ do
  let vkTransitionImageLayoutEXTPtr = pVkTransitionImageLayoutEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkTransitionImageLayoutEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkTransitionImageLayoutEXT is null" Nothing Nothing
  let vkTransitionImageLayoutEXT' = mkVkTransitionImageLayoutEXT vkTransitionImageLayoutEXTPtr
  pPTransitions <- ContT $ allocaBytes @HostImageLayoutTransitionInfoEXT ((Data.Vector.length (transitions)) * 56)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPTransitions `plusPtr` (56 * (i)) :: Ptr HostImageLayoutTransitionInfoEXT) (e)) (transitions)
  r <- lift $ traceAroundEvent "vkTransitionImageLayoutEXT" (vkTransitionImageLayoutEXT'
                                                               (deviceHandle (device))
                                                               ((fromIntegral (Data.Vector.length $ (transitions)) :: Word32))
                                                               (pPTransitions))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- No documentation found for TopLevel "vkGetImageSubresourceLayout2EXT"
getImageSubresourceLayout2EXT = getImageSubresourceLayout2KHR


-- | VkPhysicalDeviceHostImageCopyFeaturesEXT - Structure indicating support
-- for copies to or from images from host memory
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceHostImageCopyFeaturesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceHostImageCopyFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceHostImageCopyFeaturesEXT = PhysicalDeviceHostImageCopyFeaturesEXT
  { -- | #features-hostImageCopy# @hostImageCopy@ indicates that the
    -- implementation supports copying from host memory to images using the
    -- 'copyMemoryToImageEXT' command, copying from images to host memory using
    -- the 'copyImageToMemoryEXT' command, and copying between images using the
    -- 'copyImageToImageEXT' command.
    hostImageCopy :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceHostImageCopyFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceHostImageCopyFeaturesEXT

instance ToCStruct PhysicalDeviceHostImageCopyFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceHostImageCopyFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (hostImageCopy))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceHostImageCopyFeaturesEXT where
  peekCStruct p = do
    hostImageCopy <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceHostImageCopyFeaturesEXT
             (bool32ToBool hostImageCopy)

instance Storable PhysicalDeviceHostImageCopyFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceHostImageCopyFeaturesEXT where
  zero = PhysicalDeviceHostImageCopyFeaturesEXT
           zero


-- | VkPhysicalDeviceHostImageCopyPropertiesEXT - Structure enumerating image
-- layouts supported by an implementation for host memory copies
--
-- = Description
--
-- If the 'PhysicalDeviceHostImageCopyPropertiesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- If @pCopyDstLayouts@ is @NULL@, then the number of image layouts that
-- are supported in 'CopyMemoryToImageInfoEXT'::@dstImageLayout@ and
-- 'CopyImageToImageInfoEXT'::@dstImageLayout@ is returned in
-- @copyDstLayoutCount@. Otherwise, @copyDstLayoutCount@ /must/ be set by
-- the user to the number of elements in the @pCopyDstLayouts@ array, and
-- on return the variable is overwritten with the number of values actually
-- written to @pCopyDstLayouts@. If the value of @copyDstLayoutCount@ is
-- less than the number of image layouts that are supported, at most
-- @copyDstLayoutCount@ values will be written to @pCopyDstLayouts@. The
-- implementation /must/ include the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' image layout in
-- @pCopyDstLayouts@.
--
-- If @pCopySrcLayouts@ is @NULL@, then the number of image layouts that
-- are supported in 'CopyImageToMemoryInfoEXT'::@srcImageLayout@ and
-- 'CopyImageToImageInfoEXT'::@srcImageLayout@ is returned in
-- @copySrcLayoutCount@. Otherwise, @copySrcLayoutCount@ /must/ be set by
-- the user to the number of elements in the @pCopySrcLayouts@ array, and
-- on return the variable is overwritten with the number of values actually
-- written to @pCopySrcLayouts@. If the value of @copySrcLayoutCount@ is
-- less than the number of image layouts that are supported, at most
-- @copySrcLayoutCount@ values will be written to @pCopySrcLayouts@. The
-- implementation /must/ include the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' image layout in
-- @pCopySrcLayouts@.
--
-- The @optimalTilingLayoutUUID@ value can be used to ensure compatible
-- data layouts when using the 'HOST_IMAGE_COPY_MEMCPY_EXT' flag in
-- 'copyMemoryToImageEXT' and 'copyImageToMemoryEXT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceHostImageCopyPropertiesEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT'
--
-- -   #VUID-VkPhysicalDeviceHostImageCopyPropertiesEXT-pCopySrcLayouts-parameter#
--     If @copySrcLayoutCount@ is not @0@, and @pCopySrcLayouts@ is not
--     @NULL@, @pCopySrcLayouts@ /must/ be a valid pointer to an array of
--     @copySrcLayoutCount@ 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     values
--
-- -   #VUID-VkPhysicalDeviceHostImageCopyPropertiesEXT-pCopyDstLayouts-parameter#
--     If @copyDstLayoutCount@ is not @0@, and @pCopyDstLayouts@ is not
--     @NULL@, @pCopyDstLayouts@ /must/ be a valid pointer to an array of
--     @copyDstLayoutCount@ 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceHostImageCopyPropertiesEXT = PhysicalDeviceHostImageCopyPropertiesEXT
  { -- | @copySrcLayoutCount@ is an integer related to the number of image
    -- layouts for host copies from images available or queried, as described
    -- below.
    copySrcLayoutCount :: Word32
  , -- | @pCopySrcLayouts@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' in which supported image
    -- layouts for use with host copy operations from images are returned.
    copySrcLayouts :: Ptr ImageLayout
  , -- | @copyDstLayoutCount@ is an integer related to the number of image
    -- layouts for host copies to images available or queried, as described
    -- below.
    copyDstLayoutCount :: Word32
  , -- | @pCopyDstLayouts@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' in which supported image
    -- layouts for use with host copy operations to images are returned.
    copyDstLayouts :: Ptr ImageLayout
  , -- | @optimalTilingLayoutUUID@ is an array of
    -- 'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values representing a
    -- universally unique identifier for the implementation’s swizzling layout
    -- of images created with
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
    optimalTilingLayoutUUID :: ByteString
  , -- | @identicalMemoryTypeRequirements@ indicates that specifying the
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
    -- flag in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ does not affect
    -- the memory type requirements of the image.
    identicalMemoryTypeRequirements :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceHostImageCopyPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceHostImageCopyPropertiesEXT

instance ToCStruct PhysicalDeviceHostImageCopyPropertiesEXT where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceHostImageCopyPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (copySrcLayoutCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ImageLayout))) (copySrcLayouts)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (copyDstLayoutCount)
    poke ((p `plusPtr` 40 :: Ptr (Ptr ImageLayout))) (copyDstLayouts)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (FixedArray UUID_SIZE Word8))) (optimalTilingLayoutUUID)
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (identicalMemoryTypeRequirements))
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceHostImageCopyPropertiesEXT where
  peekCStruct p = do
    copySrcLayoutCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pCopySrcLayouts <- peek @(Ptr ImageLayout) ((p `plusPtr` 24 :: Ptr (Ptr ImageLayout)))
    copyDstLayoutCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pCopyDstLayouts <- peek @(Ptr ImageLayout) ((p `plusPtr` 40 :: Ptr (Ptr ImageLayout)))
    optimalTilingLayoutUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 48 :: Ptr (FixedArray UUID_SIZE Word8)))
    identicalMemoryTypeRequirements <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    pure $ PhysicalDeviceHostImageCopyPropertiesEXT
             copySrcLayoutCount
             pCopySrcLayouts
             copyDstLayoutCount
             pCopyDstLayouts
             optimalTilingLayoutUUID
             (bool32ToBool identicalMemoryTypeRequirements)

instance Storable PhysicalDeviceHostImageCopyPropertiesEXT where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceHostImageCopyPropertiesEXT where
  zero = PhysicalDeviceHostImageCopyPropertiesEXT
           zero
           zero
           zero
           zero
           mempty
           zero


-- | VkMemoryToImageCopyEXT - Structure specifying a host memory to image
-- copy operation
--
-- = Description
--
-- This structure is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
-- except it defines host memory as the source of copy instead of a buffer.
-- In particular, the same data packing rules and restrictions as that
-- structure apply here as well.
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryToImageCopyEXT-pHostPointer-09061# @pHostPointer@
--     /must/ point to memory that is large enough to contain all memory
--     locations that are accessed according to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-VkMemoryToImageCopyEXT-pRegions-09062# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkMemoryToImageCopyEXT-memoryRowLength-09101#
--     @memoryRowLength@ /must/ be @0@, or greater than or equal to the
--     @width@ member of @imageExtent@
--
-- -   #VUID-VkMemoryToImageCopyEXT-memoryImageHeight-09102#
--     @memoryImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   #VUID-VkMemoryToImageCopyEXT-aspectMask-09103# The @aspectMask@
--     member of @imageSubresource@ /must/ only have a single bit set
--
-- -   #VUID-VkMemoryToImageCopyEXT-imageExtent-06659# @imageExtent.width@
--     /must/ not be 0
--
-- -   #VUID-VkMemoryToImageCopyEXT-imageExtent-06660# @imageExtent.height@
--     /must/ not be 0
--
-- -   #VUID-VkMemoryToImageCopyEXT-imageExtent-06661# @imageExtent.depth@
--     /must/ not be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryToImageCopyEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT'
--
-- -   #VUID-VkMemoryToImageCopyEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkMemoryToImageCopyEXT-pHostPointer-parameter# @pHostPointer@
--     /must/ be a pointer value
--
-- -   #VUID-VkMemoryToImageCopyEXT-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'CopyMemoryToImageInfoEXT', 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryToImageCopyEXT = MemoryToImageCopyEXT
  { -- | @pHostPointer@ is the host memory address which is the source of the
    -- copy.
    hostPointer :: Ptr ()
  , -- | @memoryRowLength@ and @memoryImageHeight@ specify in texels a subregion
    -- of a larger two- or three-dimensional image in host memory, and control
    -- the addressing calculations. If either of these values is zero, that
    -- aspect of the host memory is considered to be tightly packed according
    -- to the @imageExtent@.
    memoryRowLength :: Word32
  , -- No documentation found for Nested "VkMemoryToImageCopyEXT" "memoryImageHeight"
    memoryImageHeight :: Word32
  , -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' used to
    -- specify the specific image subresources of the image used for the source
    -- or destination image data.
    imageSubresource :: ImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
    -- sub-region of the destination image data.
    imageOffset :: Offset3D
  , -- | @imageExtent@ is the size in texels of the image to copy in @width@,
    -- @height@ and @depth@.
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryToImageCopyEXT)
#endif
deriving instance Show MemoryToImageCopyEXT

instance ToCStruct MemoryToImageCopyEXT where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryToImageCopyEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (hostPointer)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (memoryRowLength)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (memoryImageHeight)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    poke ((p `plusPtr` 48 :: Ptr Offset3D)) (imageOffset)
    poke ((p `plusPtr` 60 :: Ptr Extent3D)) (imageExtent)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct MemoryToImageCopyEXT where
  peekCStruct p = do
    pHostPointer <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    memoryRowLength <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    memoryImageHeight <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 48 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 60 :: Ptr Extent3D))
    pure $ MemoryToImageCopyEXT
             pHostPointer
             memoryRowLength
             memoryImageHeight
             imageSubresource
             imageOffset
             imageExtent

instance Storable MemoryToImageCopyEXT where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryToImageCopyEXT where
  zero = MemoryToImageCopyEXT
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkImageToMemoryCopyEXT - Structure specifying an image to host memory
-- copy operation
--
-- = Description
--
-- This structure is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
-- except it defines host memory as the target of copy instead of a buffer.
-- In particular, the same data packing rules and restrictions as that
-- structure apply here as well.
--
-- == Valid Usage
--
-- -   #VUID-VkImageToMemoryCopyEXT-pHostPointer-09066# @pHostPointer@
--     /must/ point to memory that is large enough to contain all memory
--     locations that are accessed according to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-VkImageToMemoryCopyEXT-pRegions-09067# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkImageToMemoryCopyEXT-memoryRowLength-09101#
--     @memoryRowLength@ /must/ be @0@, or greater than or equal to the
--     @width@ member of @imageExtent@
--
-- -   #VUID-VkImageToMemoryCopyEXT-memoryImageHeight-09102#
--     @memoryImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   #VUID-VkImageToMemoryCopyEXT-aspectMask-09103# The @aspectMask@
--     member of @imageSubresource@ /must/ only have a single bit set
--
-- -   #VUID-VkImageToMemoryCopyEXT-imageExtent-06659# @imageExtent.width@
--     /must/ not be 0
--
-- -   #VUID-VkImageToMemoryCopyEXT-imageExtent-06660# @imageExtent.height@
--     /must/ not be 0
--
-- -   #VUID-VkImageToMemoryCopyEXT-imageExtent-06661# @imageExtent.depth@
--     /must/ not be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageToMemoryCopyEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT'
--
-- -   #VUID-VkImageToMemoryCopyEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkImageToMemoryCopyEXT-pHostPointer-parameter# @pHostPointer@
--     /must/ be a pointer value
--
-- -   #VUID-VkImageToMemoryCopyEXT-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'CopyImageToMemoryInfoEXT', 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageToMemoryCopyEXT = ImageToMemoryCopyEXT
  { -- | @pHostPointer@ is the host memory address which is the destination of
    -- the copy.
    hostPointer :: Ptr ()
  , -- | @memoryRowLength@ and @memoryImageHeight@ specify in texels a subregion
    -- of a larger two- or three-dimensional image in host memory, and control
    -- the addressing calculations. If either of these values is zero, that
    -- aspect of the host memory is considered to be tightly packed according
    -- to the @imageExtent@.
    memoryRowLength :: Word32
  , -- No documentation found for Nested "VkImageToMemoryCopyEXT" "memoryImageHeight"
    memoryImageHeight :: Word32
  , -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' used to
    -- specify the specific image subresources of the image used for the source
    -- or destination image data.
    imageSubresource :: ImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
    -- sub-region of the source image data.
    imageOffset :: Offset3D
  , -- | @imageExtent@ is the size in texels of the image to copy in @width@,
    -- @height@ and @depth@.
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageToMemoryCopyEXT)
#endif
deriving instance Show ImageToMemoryCopyEXT

instance ToCStruct ImageToMemoryCopyEXT where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageToMemoryCopyEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (hostPointer)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (memoryRowLength)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (memoryImageHeight)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    poke ((p `plusPtr` 48 :: Ptr Offset3D)) (imageOffset)
    poke ((p `plusPtr` 60 :: Ptr Extent3D)) (imageExtent)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageToMemoryCopyEXT where
  peekCStruct p = do
    pHostPointer <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    memoryRowLength <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    memoryImageHeight <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 48 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 60 :: Ptr Extent3D))
    pure $ ImageToMemoryCopyEXT
             pHostPointer
             memoryRowLength
             memoryImageHeight
             imageSubresource
             imageOffset
             imageExtent

instance Storable ImageToMemoryCopyEXT where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageToMemoryCopyEXT where
  zero = ImageToMemoryCopyEXT
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkCopyMemoryToImageInfoEXT - Structure specifying parameters of host
-- memory to image copy command
--
-- = Description
--
-- 'copyMemoryToImageEXT' does not check whether the device memory
-- associated with @dstImage@ is currently in use before performing the
-- copy. The application /must/ guarantee that any previously submitted
-- command that reads from or writes to the copy regions has completed
-- before the host performs the copy.
--
-- Copy regions for the image /must/ be aligned to a multiple of the texel
-- block extent in each dimension, except at the edges of the image, where
-- region extents /must/ match the edge of the image.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-09109# If @dstImage@ is
--     sparse then all memory ranges accessed by the copy command /must/ be
--     bound as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-resource-binding Binding Resource Memory>
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-09111# If the stencil
--     aspect of @dstImage@ is accessed, and @dstImage@ was not created
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @dstImage@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-09112# If the stencil
--     aspect of @dstImage@ is accessed, and @dstImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @dstImage@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-09113# If non-stencil
--     aspects of @dstImage@ are accessed, @dstImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-imageOffset-09114# If @flags@
--     contains 'HOST_IMAGE_COPY_MEMCPY_EXT', the @x@, @y@, and @z@ members
--     of the @imageOffset@ member of each element of @pRegions@ /must/ be
--     @0@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-09115# If @flags@ contains
--     'HOST_IMAGE_COPY_MEMCPY_EXT', the @imageExtent@ member of each
--     element of @pRegions@ /must/ equal the extents of @dstImage@
--     identified by @imageSubresource@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07966# If @dstImage@ is
--     non-sparse then the image or the specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-imageSubresource-07967# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-imageSubresource-07968# If
--     @imageSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07969# @dstImage@ /must/
--     not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-imageSubresource-07970# The image
--     region specified by each element of @pRegions@ /must/ be contained
--     within the specified @imageSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-imageSubresource-07971# For each
--     element of @pRegions@, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-imageSubresource-07972# For each
--     element of @pRegions@, @imageOffset.y@ and (@imageExtent.height@ +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07973# @dstImage@ /must/
--     have a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07979# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-imageOffset-09104# For each element
--     of @pRegions@, @imageOffset.z@ and (@imageExtent.depth@ +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07980# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07274# For each element of
--     @pRegions@, @imageOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07275# For each element of
--     @pRegions@, @imageOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07276# For each element of
--     @pRegions@, @imageOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-00207# For each element of
--     @pRegions@, if the sum of @imageOffset.x@ and @extent.width@ does
--     not equal the width of the subresource specified by
--     @srcSubresource@, @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-00208# For each element of
--     @pRegions@, if the sum of @imageOffset.y@ and @extent.height@ does
--     not equal the height of the subresource specified by
--     @srcSubresource@, @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-00209# For each element of
--     @pRegions@, if the sum of @imageOffset.z@ and @extent.depth@ does
--     not equal the depth of the subresource specified by
--     @srcSubresource@, @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-imageSubresource-09105# For each
--     element of @pRegions@, @imageSubresource.aspectMask@ /must/ specify
--     aspects present in @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07981# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-07983# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be @0@ and
--     @imageSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-memoryRowLength-09106# For each
--     element of @pRegions@, @memoryRowLength@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-memoryImageHeight-09107# For each
--     element of @pRegions@, @memoryImageHeight@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-memoryRowLength-09108# For each
--     element of @pRegions@, @memoryRowLength@ divided by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     and then multiplied by the texel block size of @dstImage@ /must/ be
--     less than or equal to 231-1
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImageLayout-09059#
--     @dstImageLayout@ /must/ specify the current layout of the image
--     subresources of @dstImage@ specified in @pRegions@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImageLayout-09060#
--     @dstImageLayout@ /must/ be one of the image layouts returned in
--     'PhysicalDeviceHostImageCopyPropertiesEXT'::@pCopyDstLayouts@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-flags-09393# If @flags@ includes
--     'HOST_IMAGE_COPY_MEMCPY_EXT', for each region in @pRegions@,
--     @memoryRowLength@ and @memoryImageHeight@ /must/ both be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT'
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-flags-parameter# @flags@ /must/ be
--     a valid combination of 'HostImageCopyFlagBitsEXT' values
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImage-parameter# @dstImage@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-pRegions-parameter# @pRegions@
--     /must/ be a valid pointer to an array of @regionCount@ valid
--     'MemoryToImageCopyEXT' structures
--
-- -   #VUID-VkCopyMemoryToImageInfoEXT-regionCount-arraylength#
--     @regionCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'HostImageCopyFlagsEXT', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'MemoryToImageCopyEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'copyMemoryToImageEXT'
data CopyMemoryToImageInfoEXT = CopyMemoryToImageInfoEXT
  { -- | @flags@ is a bitmask of 'HostImageCopyFlagBitsEXT' values describing
    -- additional copy parameters.
    flags :: HostImageCopyFlagsEXT
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the copy.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'MemoryToImageCopyEXT' structures
    -- specifying the regions to copy.
    regions :: Vector MemoryToImageCopyEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryToImageInfoEXT)
#endif
deriving instance Show CopyMemoryToImageInfoEXT

instance ToCStruct CopyMemoryToImageInfoEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToImageInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr HostImageCopyFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @MemoryToImageCopyEXT ((Data.Vector.length (regions)) * 72)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (72 * (i)) :: Ptr MemoryToImageCopyEXT) (e)) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr MemoryToImageCopyEXT))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyMemoryToImageInfoEXT where
  peekCStruct p = do
    flags <- peek @HostImageCopyFlagsEXT ((p `plusPtr` 16 :: Ptr HostImageCopyFlagsEXT))
    dstImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pRegions <- peek @(Ptr MemoryToImageCopyEXT) ((p `plusPtr` 40 :: Ptr (Ptr MemoryToImageCopyEXT)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @MemoryToImageCopyEXT ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr MemoryToImageCopyEXT)))
    pure $ CopyMemoryToImageInfoEXT
             flags dstImage dstImageLayout pRegions'

instance Zero CopyMemoryToImageInfoEXT where
  zero = CopyMemoryToImageInfoEXT
           zero
           zero
           zero
           mempty


-- | VkCopyImageToMemoryInfoEXT - Structure specifying parameters of an image
-- to host memory copy command
--
-- = Description
--
-- 'copyImageToMemoryEXT' does not check whether the device memory
-- associated with @srcImage@ is currently in use before performing the
-- copy. The application /must/ guarantee that any previously submitted
-- command that writes to the copy regions has completed before the host
-- performs the copy.
--
-- Copy regions for the image /must/ be aligned to a multiple of the texel
-- block extent in each dimension, except at the edges of the image, where
-- region extents /must/ match the edge of the image.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-09109# If @srcImage@ is
--     sparse then all memory ranges accessed by the copy command /must/ be
--     bound as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-resource-binding Binding Resource Memory>
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-09111# If the stencil
--     aspect of @srcImage@ is accessed, and @srcImage@ was not created
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @srcImage@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-09112# If the stencil
--     aspect of @srcImage@ is accessed, and @srcImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @srcImage@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-09113# If non-stencil
--     aspects of @srcImage@ are accessed, @srcImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-imageOffset-09114# If @flags@
--     contains 'HOST_IMAGE_COPY_MEMCPY_EXT', the @x@, @y@, and @z@ members
--     of the @imageOffset@ member of each element of @pRegions@ /must/ be
--     @0@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-09115# If @flags@ contains
--     'HOST_IMAGE_COPY_MEMCPY_EXT', the @imageExtent@ member of each
--     element of @pRegions@ /must/ equal the extents of @srcImage@
--     identified by @imageSubresource@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07966# If @srcImage@ is
--     non-sparse then the image or the specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-imageSubresource-07967# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-imageSubresource-07968# If
--     @imageSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07969# @srcImage@ /must/
--     not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-imageSubresource-07970# The image
--     region specified by each element of @pRegions@ /must/ be contained
--     within the specified @imageSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-imageSubresource-07971# For each
--     element of @pRegions@, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-imageSubresource-07972# For each
--     element of @pRegions@, @imageOffset.y@ and (@imageExtent.height@ +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07973# @srcImage@ /must/
--     have a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07979# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-imageOffset-09104# For each element
--     of @pRegions@, @imageOffset.z@ and (@imageExtent.depth@ +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07980# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07274# For each element of
--     @pRegions@, @imageOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07275# For each element of
--     @pRegions@, @imageOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07276# For each element of
--     @pRegions@, @imageOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-00207# For each element of
--     @pRegions@, if the sum of @imageOffset.x@ and @extent.width@ does
--     not equal the width of the subresource specified by
--     @srcSubresource@, @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-00208# For each element of
--     @pRegions@, if the sum of @imageOffset.y@ and @extent.height@ does
--     not equal the height of the subresource specified by
--     @srcSubresource@, @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-00209# For each element of
--     @pRegions@, if the sum of @imageOffset.z@ and @extent.depth@ does
--     not equal the depth of the subresource specified by
--     @srcSubresource@, @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-imageSubresource-09105# For each
--     element of @pRegions@, @imageSubresource.aspectMask@ /must/ specify
--     aspects present in @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07981# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-07983# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be @0@ and
--     @imageSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-memoryRowLength-09106# For each
--     element of @pRegions@, @memoryRowLength@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-memoryImageHeight-09107# For each
--     element of @pRegions@, @memoryImageHeight@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-memoryRowLength-09108# For each
--     element of @pRegions@, @memoryRowLength@ divided by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     and then multiplied by the texel block size of @srcImage@ /must/ be
--     less than or equal to 231-1
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImageLayout-09064#
--     @srcImageLayout@ /must/ specify the current layout of the image
--     subresources of @srcImage@ specified in @pRegions@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImageLayout-09065#
--     @srcImageLayout@ /must/ be one of the image layouts returned in
--     'PhysicalDeviceHostImageCopyPropertiesEXT'::@pCopySrcLayouts@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-flags-09394# If @flags@ includes
--     'HOST_IMAGE_COPY_MEMCPY_EXT', for each region in @pRegions@,
--     @memoryRowLength@ and @memoryImageHeight@ /must/ both be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT'
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-flags-parameter# @flags@ /must/ be
--     a valid combination of 'HostImageCopyFlagBitsEXT' values
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImage-parameter# @srcImage@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-srcImageLayout-parameter#
--     @srcImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-pRegions-parameter# @pRegions@
--     /must/ be a valid pointer to an array of @regionCount@ valid
--     'ImageToMemoryCopyEXT' structures
--
-- -   #VUID-VkCopyImageToMemoryInfoEXT-regionCount-arraylength#
--     @regionCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'HostImageCopyFlagsEXT', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'ImageToMemoryCopyEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'copyImageToMemoryEXT'
data CopyImageToMemoryInfoEXT = CopyImageToMemoryInfoEXT
  { -- | @flags@ is a bitmask of 'HostImageCopyFlagBitsEXT' values describing
    -- additional copy parameters.
    flags :: HostImageCopyFlagsEXT
  , -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- copy.
    srcImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'ImageToMemoryCopyEXT' structures
    -- specifying the regions to copy.
    regions :: Vector ImageToMemoryCopyEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageToMemoryInfoEXT)
#endif
deriving instance Show CopyImageToMemoryInfoEXT

instance ToCStruct CopyImageToMemoryInfoEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageToMemoryInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr HostImageCopyFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @ImageToMemoryCopyEXT ((Data.Vector.length (regions)) * 72)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (72 * (i)) :: Ptr ImageToMemoryCopyEXT) (e)) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ImageToMemoryCopyEXT))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyImageToMemoryInfoEXT where
  peekCStruct p = do
    flags <- peek @HostImageCopyFlagsEXT ((p `plusPtr` 16 :: Ptr HostImageCopyFlagsEXT))
    srcImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageToMemoryCopyEXT) ((p `plusPtr` 40 :: Ptr (Ptr ImageToMemoryCopyEXT)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageToMemoryCopyEXT ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr ImageToMemoryCopyEXT)))
    pure $ CopyImageToMemoryInfoEXT
             flags srcImage srcImageLayout pRegions'

instance Zero CopyImageToMemoryInfoEXT where
  zero = CopyImageToMemoryInfoEXT
           zero
           zero
           zero
           mempty


-- | VkCopyImageToImageInfoEXT - Structure specifying parameters of an image
-- to image host copy command
--
-- = Description
--
-- 'copyImageToImageEXT' does not check whether the device memory
-- associated with @srcImage@ or @dstImage@ is currently in use before
-- performing the copy. The application /must/ guarantee that any
-- previously submitted command that writes to the copy regions has
-- completed before the host performs the copy.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-09069# @srcImage@ and
--     @dstImage@ /must/ have been created with identical image creation
--     parameters
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-09109# If @srcImage@ is
--     sparse then all memory ranges accessed by the copy command /must/ be
--     bound as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-resource-binding Binding Resource Memory>
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-09111# If the stencil
--     aspect of @srcImage@ is accessed, and @srcImage@ was not created
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @srcImage@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-09112# If the stencil
--     aspect of @srcImage@ is accessed, and @srcImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @srcImage@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-09113# If non-stencil
--     aspects of @srcImage@ are accessed, @srcImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcOffset-09114# If @flags@ contains
--     'HOST_IMAGE_COPY_MEMCPY_EXT', the @x@, @y@, and @z@ members of the
--     @srcOffset@ member of each element of @pRegions@ /must/ be @0@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-09115# If @flags@ contains
--     'HOST_IMAGE_COPY_MEMCPY_EXT', the @extent@ member of each element of
--     @pRegions@ /must/ equal the extents of @srcImage@ identified by
--     @srcSubresource@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07966# If @srcImage@ is
--     non-sparse then the image or the specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcSubresource-07967# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcSubresource-07968# If
--     @srcSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07969# @srcImage@ /must/
--     not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcSubresource-07970# The image
--     region specified by each element of @pRegions@ /must/ be contained
--     within the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcSubresource-07971# For each
--     element of @pRegions@, @srcOffset.x@ and (@extent.width@ +
--     @srcOffset.x@) /must/ both be greater than or equal to @0@ and less
--     than or equal to the width of the specified @srcSubresource@ of
--     @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcSubresource-07972# For each
--     element of @pRegions@, @srcOffset.y@ and (@extent.height@ +
--     @srcOffset.y@) /must/ both be greater than or equal to @0@ and less
--     than or equal to the height of the specified @srcSubresource@ of
--     @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07979# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @srcOffset.y@ /must/ be @0@ and
--     @extent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcOffset-09104# For each element of
--     @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07980# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07274# For each element of
--     @pRegions@, @srcOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07275# For each element of
--     @pRegions@, @srcOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07276# For each element of
--     @pRegions@, @srcOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-00207# For each element of
--     @pRegions@, if the sum of @srcOffset.x@ and @extent.width@ does not
--     equal the width of the subresource specified by @srcSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-00208# For each element of
--     @pRegions@, if the sum of @srcOffset.y@ and @extent.height@ does not
--     equal the height of the subresource specified by @srcSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-00209# For each element of
--     @pRegions@, if the sum of @srcOffset.z@ and @extent.depth@ does not
--     equal the depth of the subresource specified by @srcSubresource@,
--     @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcSubresource-09105# For each
--     element of @pRegions@, @srcSubresource.aspectMask@ /must/ specify
--     aspects present in @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07981# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then for each element of @pRegions@, @srcSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-07983# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @srcSubresource.baseArrayLayer@ /must/ be @0@ and
--     @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-09109# If @dstImage@ is
--     sparse then all memory ranges accessed by the copy command /must/ be
--     bound as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-resource-binding Binding Resource Memory>
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-09111# If the stencil
--     aspect of @dstImage@ is accessed, and @dstImage@ was not created
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @dstImage@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-09112# If the stencil
--     aspect of @dstImage@ is accessed, and @dstImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @dstImage@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-09113# If non-stencil
--     aspects of @dstImage@ are accessed, @dstImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstOffset-09114# If @flags@ contains
--     'HOST_IMAGE_COPY_MEMCPY_EXT', the @x@, @y@, and @z@ members of the
--     @dstOffset@ member of each element of @pRegions@ /must/ be @0@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-09115# If @flags@ contains
--     'HOST_IMAGE_COPY_MEMCPY_EXT', the @extent@ member of each element of
--     @pRegions@ /must/ equal the extents of @dstImage@ identified by
--     @dstSubresource@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07966# If @dstImage@ is
--     non-sparse then the image or the specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstSubresource-07967# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstSubresource-07968# If
--     @dstSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07969# @dstImage@ /must/
--     not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstSubresource-07970# The image
--     region specified by each element of @pRegions@ /must/ be contained
--     within the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstSubresource-07971# For each
--     element of @pRegions@, @dstOffset.x@ and (@extent.width@ +
--     @dstOffset.x@) /must/ both be greater than or equal to @0@ and less
--     than or equal to the width of the specified @dstSubresource@ of
--     @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstSubresource-07972# For each
--     element of @pRegions@, @dstOffset.y@ and (@extent.height@ +
--     @dstOffset.y@) /must/ both be greater than or equal to @0@ and less
--     than or equal to the height of the specified @dstSubresource@ of
--     @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07979# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @dstOffset.y@ /must/ be @0@ and
--     @extent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstOffset-09104# For each element of
--     @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07980# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07274# For each element of
--     @pRegions@, @dstOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07275# For each element of
--     @pRegions@, @dstOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07276# For each element of
--     @pRegions@, @dstOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-00207# For each element of
--     @pRegions@, if the sum of @dstOffset.x@ and @extent.width@ does not
--     equal the width of the subresource specified by @srcSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-00208# For each element of
--     @pRegions@, if the sum of @dstOffset.y@ and @extent.height@ does not
--     equal the height of the subresource specified by @srcSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-00209# For each element of
--     @pRegions@, if the sum of @dstOffset.z@ and @extent.depth@ does not
--     equal the depth of the subresource specified by @srcSubresource@,
--     @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstSubresource-09105# For each
--     element of @pRegions@, @dstSubresource.aspectMask@ /must/ specify
--     aspects present in @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07981# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then for each element of @pRegions@, @dstSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-07983# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and
--     @dstSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImageLayout-09070#
--     @srcImageLayout@ /must/ specify the current layout of the image
--     subresources of @srcImage@ specified in @pRegions@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImageLayout-09071#
--     @dstImageLayout@ /must/ specify the current layout of the image
--     subresources of @dstImage@ specified in @pRegions@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImageLayout-09072#
--     @srcImageLayout@ /must/ be one of the image layouts returned in
--     'PhysicalDeviceHostImageCopyPropertiesEXT'::@pCopySrcLayouts@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImageLayout-09073#
--     @dstImageLayout@ /must/ be one of the image layouts returned in
--     'PhysicalDeviceHostImageCopyPropertiesEXT'::@pCopyDstLayouts@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyImageToImageInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT'
--
-- -   #VUID-VkCopyImageToImageInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-flags-parameter# @flags@ /must/ be a
--     valid combination of 'HostImageCopyFlagBitsEXT' values
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImage-parameter# @srcImage@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageToImageInfoEXT-srcImageLayout-parameter#
--     @srcImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImage-parameter# @dstImage@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageToImageInfoEXT-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyImageToImageInfoEXT-pRegions-parameter# @pRegions@
--     /must/ be a valid pointer to an array of @regionCount@ valid
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2'
--     structures
--
-- -   #VUID-VkCopyImageToImageInfoEXT-regionCount-arraylength#
--     @regionCount@ /must/ be greater than @0@
--
-- -   #VUID-VkCopyImageToImageInfoEXT-commonparent# Both of @dstImage@,
--     and @srcImage@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'HostImageCopyFlagsEXT', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'copyImageToImageEXT'
data CopyImageToImageInfoEXT = CopyImageToImageInfoEXT
  { -- | @flags@ is a bitmask of 'HostImageCopyFlagBitsEXT' values describing
    -- additional copy parameters.
    flags :: HostImageCopyFlagsEXT
  , -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- copy.
    srcImageLayout :: ImageLayout
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the copy.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2'
    -- structures specifying the regions to copy.
    regions :: Vector ImageCopy2
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageToImageInfoEXT)
#endif
deriving instance Show CopyImageToImageInfoEXT

instance ToCStruct CopyImageToImageInfoEXT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageToImageInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr HostImageCopyFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 40 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @ImageCopy2 ((Data.Vector.length (regions)) * 88)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageCopy2) (e)) (regions)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr ImageCopy2))) (pPRegions')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyImageToImageInfoEXT where
  peekCStruct p = do
    flags <- peek @HostImageCopyFlagsEXT ((p `plusPtr` 16 :: Ptr HostImageCopyFlagsEXT))
    srcImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 40 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 48 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageCopy2) ((p `plusPtr` 56 :: Ptr (Ptr ImageCopy2)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageCopy2 ((pRegions `advancePtrBytes` (88 * (i)) :: Ptr ImageCopy2)))
    pure $ CopyImageToImageInfoEXT
             flags srcImage srcImageLayout dstImage dstImageLayout pRegions'

instance Zero CopyImageToImageInfoEXT where
  zero = CopyImageToImageInfoEXT
           zero
           zero
           zero
           zero
           zero
           mempty


-- | VkHostImageLayoutTransitionInfoEXT - Structure specifying the parameters
-- of a host-side image layout transition
--
-- = Description
--
-- 'transitionImageLayoutEXT' does not check whether the device memory
-- associated with an image is currently in use before performing the
-- layout transition. The application /must/ guarantee that any previously
-- submitted command that reads from or writes to this subresource has
-- completed before the host performs the layout transition.
--
-- Note
--
-- Image layout transitions performed on the host do not require queue
-- family ownership transfers as the physical layout of the image will not
-- vary between queue families for the layouts supported by this function.
--
-- == Valid Usage
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-image-09055# @image@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-subresourceRange-01486#
--     @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-subresourceRange-01724# If
--     @subresourceRange.levelCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-subresourceRange-01488#
--     @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @image@ was created
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-subresourceRange-01725# If
--     @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-image-01932# If @image@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-image-09241# If @image@ has
--     a color format that is single-plane, then the @aspectMask@ member of
--     @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-image-09242# If @image@ has
--     a color format and is not /disjoint/, then the @aspectMask@ member
--     of @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-image-01672# If @image@ has
--     a multi-planar format and the image is /disjoint/, then the
--     @aspectMask@ member of @subresourceRange@ /must/ include at least
--     one
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-image-03319# If @image@ has
--     a depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include either or both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-image-03320# If @image@ has
--     a depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-aspectMask-08702# If the
--     @aspectMask@ member of @subresourceRange@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     @oldLayout@ and @newLayout@ /must/ not be one of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-aspectMask-08703# If the
--     @aspectMask@ member of @subresourceRange@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     @oldLayout@ and @newLayout@ /must/ not be one of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-oldLayout-09229#
--     @oldLayout@ /must/ be either
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or the
--     current layout of the image subresources as specified in
--     @subresourceRange@
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-oldLayout-09230# If
--     @oldLayout@ is not
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED', it
--     /must/ be one of the layouts in
--     'PhysicalDeviceHostImageCopyPropertiesEXT'::@pCopySrcLayouts@
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-newLayout-09057#
--     @newLayout@ /must/ be one of the layouts in
--     'PhysicalDeviceHostImageCopyPropertiesEXT'::@pCopyDstLayouts@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-image-parameter# @image@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-oldLayout-parameter#
--     @oldLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-newLayout-parameter#
--     @newLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkHostImageLayoutTransitionInfoEXT-subresourceRange-parameter#
--     @subresourceRange@ /must/ be a valid
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.ImageView.ImageSubresourceRange',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'transitionImageLayoutEXT'
data HostImageLayoutTransitionInfoEXT = HostImageLayoutTransitionInfoEXT
  { -- | @image@ is a handle to the image affected by this layout transition.
    image :: Image
  , -- | @oldLayout@ is the old layout in an
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    oldLayout :: ImageLayout
  , -- | @newLayout@ is the new layout in an
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    newLayout :: ImageLayout
  , -- | @subresourceRange@ describes the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views image subresource range>
    -- within @image@ that is affected by this layout transition.
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HostImageLayoutTransitionInfoEXT)
#endif
deriving instance Show HostImageLayoutTransitionInfoEXT

instance ToCStruct HostImageLayoutTransitionInfoEXT where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HostImageLayoutTransitionInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (oldLayout)
    poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (newLayout)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceRange)) (subresourceRange)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceRange)) (zero)
    f

instance FromCStruct HostImageLayoutTransitionInfoEXT where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    oldLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    newLayout <- peek @ImageLayout ((p `plusPtr` 28 :: Ptr ImageLayout))
    subresourceRange <- peekCStruct @ImageSubresourceRange ((p `plusPtr` 32 :: Ptr ImageSubresourceRange))
    pure $ HostImageLayoutTransitionInfoEXT
             image oldLayout newLayout subresourceRange

instance Storable HostImageLayoutTransitionInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HostImageLayoutTransitionInfoEXT where
  zero = HostImageLayoutTransitionInfoEXT
           zero
           zero
           zero
           zero


-- | VkSubresourceHostMemcpySizeEXT - Memory size needed to copy to or from
-- an image on the host with VK_HOST_IMAGE_COPY_MEMCPY_EXT
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubresourceHostMemcpySizeEXT = SubresourceHostMemcpySizeEXT
  { -- | @size@ is the size in bytes of the image subresource.
    size :: DeviceSize }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubresourceHostMemcpySizeEXT)
#endif
deriving instance Show SubresourceHostMemcpySizeEXT

instance ToCStruct SubresourceHostMemcpySizeEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubresourceHostMemcpySizeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SubresourceHostMemcpySizeEXT where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ SubresourceHostMemcpySizeEXT
             size

instance Storable SubresourceHostMemcpySizeEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubresourceHostMemcpySizeEXT where
  zero = SubresourceHostMemcpySizeEXT
           zero


-- | VkHostImageCopyDevicePerformanceQueryEXT - Struct containing information
-- about optimality of device access
--
-- = Description
--
-- The implementation /may/ return 'Vulkan.Core10.FundamentalTypes.FALSE'
-- in @optimalDeviceAccess@ if @identicalMemoryLayout@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'. If @identicalMemoryLayout@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', @optimalDeviceAccess@ /must/ be
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- The implementation /may/ return 'Vulkan.Core10.FundamentalTypes.TRUE' in
-- @optimalDeviceAccess@ while @identicalMemoryLayout@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'. In this situation, any device
-- performance impact /should/ not be measurable.
--
-- If
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@format@
-- is a block-compressed format and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- returns 'Vulkan.Core10.Enums.Result.SUCCESS', the implementation /must/
-- return 'Vulkan.Core10.FundamentalTypes.TRUE' in @optimalDeviceAccess@.
--
-- Note
--
-- Applications can make use of @optimalDeviceAccess@ to determine their
-- resource copying strategy. If a resource is expected to be accessed more
-- on device than on the host, and the implementation considers the
-- resource sub-optimally accessed, it is likely better to use device
-- copies instead.
--
-- Note
--
-- Layout not being identical yet still considered optimal for device
-- access could happen if the implementation has different memory layout
-- patterns, some of which are easier to access on the host.
--
-- Note
--
-- The most practical reason for @optimalDeviceAccess@ to be
-- 'Vulkan.Core10.FundamentalTypes.FALSE' is that host image access may
-- disable framebuffer compression where it would otherwise have been
-- enabled. This represents far more efficient host image access since no
-- compression algorithm is required to read or write to the image, but it
-- would impact device access performance. Some implementations may only
-- set @optimalDeviceAccess@ to 'Vulkan.Core10.FundamentalTypes.FALSE' if
-- certain conditions are met, such as specific image usage flags or
-- creation flags.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data HostImageCopyDevicePerformanceQueryEXT = HostImageCopyDevicePerformanceQueryEXT
  { -- | @optimalDeviceAccess@ returns 'Vulkan.Core10.FundamentalTypes.TRUE' if
    -- use of host image copy has no adverse effect on device access
    -- performance, compared to an image that is created with exact same
    -- creation parameters, and bound to the same
    -- 'Vulkan.Core10.Handles.DeviceMemory', except that
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
    -- is replaced with
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'.
    optimalDeviceAccess :: Bool
  , -- | @identicalMemoryLayout@ returns 'Vulkan.Core10.FundamentalTypes.TRUE' if
    -- use of host image copy has no impact on memory layout compared to an
    -- image that is created with exact same creation parameters, and bound to
    -- the same 'Vulkan.Core10.Handles.DeviceMemory', except that
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
    -- is replaced with
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'.
    identicalMemoryLayout :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HostImageCopyDevicePerformanceQueryEXT)
#endif
deriving instance Show HostImageCopyDevicePerformanceQueryEXT

instance ToCStruct HostImageCopyDevicePerformanceQueryEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HostImageCopyDevicePerformanceQueryEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (optimalDeviceAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (identicalMemoryLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct HostImageCopyDevicePerformanceQueryEXT where
  peekCStruct p = do
    optimalDeviceAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    identicalMemoryLayout <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ HostImageCopyDevicePerformanceQueryEXT
             (bool32ToBool optimalDeviceAccess)
             (bool32ToBool identicalMemoryLayout)

instance Storable HostImageCopyDevicePerformanceQueryEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HostImageCopyDevicePerformanceQueryEXT where
  zero = HostImageCopyDevicePerformanceQueryEXT
           zero
           zero


type HostImageCopyFlagsEXT = HostImageCopyFlagBitsEXT

-- | VkHostImageCopyFlagBitsEXT - Bitmask specifying additional copy
-- parameters
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- 'HostImageCopyFlagsEXT'
newtype HostImageCopyFlagBitsEXT = HostImageCopyFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'HOST_IMAGE_COPY_MEMCPY_EXT' specifies that no memory layout swizzling
-- is to be applied during data copy. For copies between memory and images,
-- this flag indicates that image data in host memory is swizzled in
-- exactly the same way as the image data on the device. Using this flag
-- indicates that the implementations /may/ use a simple memory copy to
-- transfer the data between the host memory and the device memory. The
-- format of the swizzled data in host memory is platform dependent and is
-- not defined in this specification.
pattern HOST_IMAGE_COPY_MEMCPY_EXT = HostImageCopyFlagBitsEXT 0x00000001

conNameHostImageCopyFlagBitsEXT :: String
conNameHostImageCopyFlagBitsEXT = "HostImageCopyFlagBitsEXT"

enumPrefixHostImageCopyFlagBitsEXT :: String
enumPrefixHostImageCopyFlagBitsEXT = "HOST_IMAGE_COPY_MEMCPY_EXT"

showTableHostImageCopyFlagBitsEXT :: [(HostImageCopyFlagBitsEXT, String)]
showTableHostImageCopyFlagBitsEXT = [(HOST_IMAGE_COPY_MEMCPY_EXT, "")]

instance Show HostImageCopyFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixHostImageCopyFlagBitsEXT
      showTableHostImageCopyFlagBitsEXT
      conNameHostImageCopyFlagBitsEXT
      (\(HostImageCopyFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read HostImageCopyFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixHostImageCopyFlagBitsEXT
      showTableHostImageCopyFlagBitsEXT
      conNameHostImageCopyFlagBitsEXT
      HostImageCopyFlagBitsEXT

-- No documentation found for TopLevel "VkImageSubresource2EXT"
type ImageSubresource2EXT = ImageSubresource2KHR


-- No documentation found for TopLevel "VkSubresourceLayout2EXT"
type SubresourceLayout2EXT = SubresourceLayout2KHR


type EXT_HOST_IMAGE_COPY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_HOST_IMAGE_COPY_SPEC_VERSION"
pattern EXT_HOST_IMAGE_COPY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_HOST_IMAGE_COPY_SPEC_VERSION = 1


type EXT_HOST_IMAGE_COPY_EXTENSION_NAME = "VK_EXT_host_image_copy"

-- No documentation found for TopLevel "VK_EXT_HOST_IMAGE_COPY_EXTENSION_NAME"
pattern EXT_HOST_IMAGE_COPY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HOST_IMAGE_COPY_EXTENSION_NAME = "VK_EXT_host_image_copy"

