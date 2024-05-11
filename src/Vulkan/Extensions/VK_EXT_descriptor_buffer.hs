{-# language CPP #-}
-- | = Name
--
-- VK_EXT_descriptor_buffer - device extension
--
-- == VK_EXT_descriptor_buffer
--
-- [__Name String__]
--     @VK_EXT_descriptor_buffer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     317
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_indexing VK_EXT_descriptor_indexing>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_descriptor_buffer] @tobski%0A*Here describe the issue or question you have about the VK_EXT_descriptor_buffer extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_descriptor_buffer.adoc VK_EXT_descriptor_buffer>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-07
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Stu Smith, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Boris Zanin, AMD
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Connor Abbott, Valve
--
--     -   Baldur Karlsson, Valve
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Rodrigo Locatti, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jeff Leger, QUALCOMM
--
--     -   Lionel Landwerlin, Intel
--
--     -   Slawomir Grajewski, Intel
--
-- == Description
--
-- This extension introduces new commands to put shader-accessible
-- descriptors directly in memory, making the management of descriptor data
-- more explicit.
--
-- == New Commands
--
-- -   'cmdBindDescriptorBufferEmbeddedSamplersEXT'
--
-- -   'cmdBindDescriptorBuffersEXT'
--
-- -   'cmdSetDescriptorBufferOffsetsEXT'
--
-- -   'getBufferOpaqueCaptureDescriptorDataEXT'
--
-- -   'getDescriptorEXT'
--
-- -   'getDescriptorSetLayoutBindingOffsetEXT'
--
-- -   'getDescriptorSetLayoutSizeEXT'
--
-- -   'getImageOpaqueCaptureDescriptorDataEXT'
--
-- -   'getImageViewOpaqueCaptureDescriptorDataEXT'
--
-- -   'getSamplerOpaqueCaptureDescriptorDataEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   'getAccelerationStructureOpaqueCaptureDescriptorDataEXT'
--
-- == New Structures
--
-- -   'BufferCaptureDescriptorDataInfoEXT'
--
-- -   'DescriptorAddressInfoEXT'
--
-- -   'DescriptorBufferBindingInfoEXT'
--
-- -   'DescriptorGetInfoEXT'
--
-- -   'ImageCaptureDescriptorDataInfoEXT'
--
-- -   'ImageViewCaptureDescriptorDataInfoEXT'
--
-- -   'SamplerCaptureDescriptorDataInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Buffer.BufferCreateInfo',
--     'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo',
--     'Vulkan.Core10.Sampler.SamplerCreateInfo',
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR',
--     'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV':
--
--     -   'OpaqueCaptureDescriptorDataCreateInfoEXT'
--
-- -   Extending 'DescriptorBufferBindingInfoEXT':
--
--     -   'DescriptorBufferBindingPushDescriptorBufferHandleEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorBufferFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT'
--
--     -   'PhysicalDeviceDescriptorBufferPropertiesEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   'AccelerationStructureCaptureDescriptorDataInfoEXT'
--
-- == New Unions
--
-- -   'DescriptorDataEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME'
--
-- -   'EXT_DESCRIPTOR_BUFFER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_EMBEDDED_IMMUTABLE_SAMPLERS_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_DENSITY_MAP_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-06-07 (Stu Smith)
--
--     -   Initial revision
--
-- == See Also
--
-- 'BufferCaptureDescriptorDataInfoEXT', 'DescriptorAddressInfoEXT',
-- 'DescriptorBufferBindingInfoEXT',
-- 'DescriptorBufferBindingPushDescriptorBufferHandleEXT',
-- 'DescriptorDataEXT', 'DescriptorGetInfoEXT',
-- 'ImageCaptureDescriptorDataInfoEXT',
-- 'ImageViewCaptureDescriptorDataInfoEXT',
-- 'OpaqueCaptureDescriptorDataCreateInfoEXT',
-- 'PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT',
-- 'PhysicalDeviceDescriptorBufferFeaturesEXT',
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT',
-- 'SamplerCaptureDescriptorDataInfoEXT',
-- 'cmdBindDescriptorBufferEmbeddedSamplersEXT',
-- 'cmdBindDescriptorBuffersEXT', 'cmdSetDescriptorBufferOffsetsEXT',
-- 'getBufferOpaqueCaptureDescriptorDataEXT', 'getDescriptorEXT',
-- 'getDescriptorSetLayoutBindingOffsetEXT',
-- 'getDescriptorSetLayoutSizeEXT',
-- 'getImageOpaqueCaptureDescriptorDataEXT',
-- 'getImageViewOpaqueCaptureDescriptorDataEXT',
-- 'getSamplerOpaqueCaptureDescriptorDataEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_descriptor_buffer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_descriptor_buffer  ( getDescriptorSetLayoutSizeEXT
                                                   , getDescriptorSetLayoutBindingOffsetEXT
                                                   , getDescriptorEXT
                                                   , cmdBindDescriptorBuffersEXT
                                                   , cmdSetDescriptorBufferOffsetsEXT
                                                   , cmdBindDescriptorBufferEmbeddedSamplersEXT
                                                   , getBufferOpaqueCaptureDescriptorDataEXT
                                                   , getImageOpaqueCaptureDescriptorDataEXT
                                                   , getImageViewOpaqueCaptureDescriptorDataEXT
                                                   , getSamplerOpaqueCaptureDescriptorDataEXT
                                                   , getAccelerationStructureOpaqueCaptureDescriptorDataEXT
                                                   , PhysicalDeviceDescriptorBufferFeaturesEXT(..)
                                                   , PhysicalDeviceDescriptorBufferPropertiesEXT(..)
                                                   , PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT(..)
                                                   , DescriptorAddressInfoEXT(..)
                                                   , DescriptorBufferBindingInfoEXT(..)
                                                   , DescriptorBufferBindingPushDescriptorBufferHandleEXT(..)
                                                   , DescriptorGetInfoEXT(..)
                                                   , BufferCaptureDescriptorDataInfoEXT(..)
                                                   , ImageCaptureDescriptorDataInfoEXT(..)
                                                   , ImageViewCaptureDescriptorDataInfoEXT(..)
                                                   , SamplerCaptureDescriptorDataInfoEXT(..)
                                                   , AccelerationStructureCaptureDescriptorDataInfoEXT(..)
                                                   , OpaqueCaptureDescriptorDataCreateInfoEXT(..)
                                                   , DescriptorDataEXT(..)
                                                   , peekDescriptorDataEXT
                                                   , EXT_DESCRIPTOR_BUFFER_SPEC_VERSION
                                                   , pattern EXT_DESCRIPTOR_BUFFER_SPEC_VERSION
                                                   , EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME
                                                   , pattern EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME
                                                   , AccelerationStructureKHR(..)
                                                   , AccelerationStructureNV(..)
                                                   , AccelerationStructureCreateFlagBitsKHR(..)
                                                   , AccelerationStructureCreateFlagsKHR
                                                   ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Marshal.Utils (with)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.Handles (AccelerationStructureKHR)
import Vulkan.Extensions.Handles (AccelerationStructureNV)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (BufferUsageFlags2CreateInfoKHR)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.DescriptorSet (DescriptorImageInfo)
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Handles (DescriptorSetLayout(..))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorBufferEmbeddedSamplersEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorBuffersEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDescriptorBufferOffsetsEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetBufferOpaqueCaptureDescriptorDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetDescriptorEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetDescriptorSetLayoutBindingOffsetEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetDescriptorSetLayoutSizeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageOpaqueCaptureDescriptorDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageViewOpaqueCaptureDescriptorDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetSamplerOpaqueCaptureDescriptorDataEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Sampler)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_INPUT_ATTACHMENT))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_SAMPLED_IMAGE))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_SAMPLER))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_STORAGE_BUFFER))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_STORAGE_IMAGE))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_UNIFORM_BUFFER))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_DENSITY_MAP_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCreateFlagsKHR)
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Extensions.Handles (AccelerationStructureNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetLayoutSizeEXT
  :: FunPtr (Ptr Device_T -> DescriptorSetLayout -> Ptr DeviceSize -> IO ()) -> Ptr Device_T -> DescriptorSetLayout -> Ptr DeviceSize -> IO ()

-- | vkGetDescriptorSetLayoutSizeEXT - Get the size of a descriptor set
-- layout in memory
--
-- = Description
--
-- The size of a descriptor set layout will be at least as large as the sum
-- total of the size of all descriptors in the layout, and /may/ be larger.
-- This size represents the amount of memory that will be required to store
-- all of the descriptors for this layout in memory, when placed according
-- to the layout’s offsets as obtained by
-- 'getDescriptorSetLayoutBindingOffsetEXT'.
--
-- If any @binding@ in @layout@ is
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT',
-- the returned size includes space for the maximum @descriptorCount@
-- descriptors as declared for that @binding@. To compute the required size
-- of a descriptor set with a
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT':
--
-- -   size = offset + descriptorSize × variableDescriptorCount
--
-- where offset is obtained by 'getDescriptorSetLayoutBindingOffsetEXT' and
-- descriptorSize is the size of the relevant descriptor as obtained from
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT', and
-- variableDescriptorCount is the equivalent of
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.DescriptorSetVariableDescriptorCountAllocateInfo'::@pDescriptorCounts@.
-- For
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK',
-- variableDescriptorCount is the size in bytes for the inline uniform
-- block, and descriptorSize is 1.
--
-- If
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT'::@combinedImageSamplerDescriptorSingleArray@
-- is 'Vulkan.Core10.FundamentalTypes.FALSE' and the variable descriptor
-- type is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
-- variableDescriptorCount is always considered to be the upper bound.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDescriptorSetLayoutSizeEXT-None-08011# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetDescriptorSetLayoutSizeEXT-layout-08012# @layout@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDescriptorSetLayoutSizeEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDescriptorSetLayoutSizeEXT-layout-parameter# @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.DescriptorSetLayout' handle
--
-- -   #VUID-vkGetDescriptorSetLayoutSizeEXT-pLayoutSizeInBytes-parameter#
--     @pLayoutSizeInBytes@ /must/ be a valid pointer to a
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' value
--
-- -   #VUID-vkGetDescriptorSetLayoutSizeEXT-layout-parent# @layout@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
getDescriptorSetLayoutSizeEXT :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that gets the size.
                                 Device
                              -> -- | @layout@ is the descriptor set layout being queried.
                                 DescriptorSetLayout
                              -> io (("layoutSizeInBytes" ::: DeviceSize))
getDescriptorSetLayoutSizeEXT device layout = liftIO . evalContT $ do
  let vkGetDescriptorSetLayoutSizeEXTPtr = pVkGetDescriptorSetLayoutSizeEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDescriptorSetLayoutSizeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDescriptorSetLayoutSizeEXT is null" Nothing Nothing
  let vkGetDescriptorSetLayoutSizeEXT' = mkVkGetDescriptorSetLayoutSizeEXT vkGetDescriptorSetLayoutSizeEXTPtr
  pPLayoutSizeInBytes <- ContT $ bracket (callocBytes @DeviceSize 8) free
  lift $ traceAroundEvent "vkGetDescriptorSetLayoutSizeEXT" (vkGetDescriptorSetLayoutSizeEXT'
                                                               (deviceHandle (device))
                                                               (layout)
                                                               (pPLayoutSizeInBytes))
  pLayoutSizeInBytes <- lift $ peek @DeviceSize pPLayoutSizeInBytes
  pure $ (pLayoutSizeInBytes)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetLayoutBindingOffsetEXT
  :: FunPtr (Ptr Device_T -> DescriptorSetLayout -> Word32 -> Ptr DeviceSize -> IO ()) -> Ptr Device_T -> DescriptorSetLayout -> Word32 -> Ptr DeviceSize -> IO ()

-- | vkGetDescriptorSetLayoutBindingOffsetEXT - Get the offset of a binding
-- within a descriptor set layout
--
-- = Description
--
-- Each binding in a descriptor set layout is assigned an offset in memory
-- by the implementation. When a shader accesses a resource with that
-- binding, it will access the bound descriptor buffer from that offset to
-- look for its descriptor. This command provides an application with that
-- offset, so that descriptors can be placed in the correct locations. The
-- precise location accessed by a shader for a given descriptor is as
-- follows:
--
-- -   location = bufferAddress + setOffset + descriptorOffset +
--     (arrayElement × descriptorSize)
--
-- where bufferAddress and setOffset are the base address and offset for
-- the identified descriptor set as specified by
-- 'cmdBindDescriptorBuffersEXT' and 'cmdSetDescriptorBufferOffsetsEXT',
-- descriptorOffset is the offset for the binding returned by this command,
-- arrayElement is the index into the array specified in the shader, and
-- descriptorSize is the size of the relevant descriptor as obtained from
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT'. Applications are
-- responsible for placing valid descriptors at the expected location in
-- order for a shader to access it. The overall offset added to
-- bufferAddress to calculate location /must/ be less than
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxSamplerDescriptorBufferRange@
-- for samplers and
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxResourceDescriptorBufferRange@
-- for resources.
--
-- If any @binding@ in @layout@ is
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT',
-- that @binding@ /must/ have the largest offset of any @binding@.
--
-- A descriptor @binding@ with type
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.DESCRIPTOR_TYPE_MUTABLE_VALVE'
-- /can/ be used. Any potential types in
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'::@pDescriptorTypes@
-- for @binding@ share the same offset. If the size of the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-mutable mutable descriptor>
-- is larger than the size of a concrete descriptor type being accessed,
-- the padding area is ignored by the implementation.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDescriptorSetLayoutBindingOffsetEXT-None-08013# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetDescriptorSetLayoutBindingOffsetEXT-layout-08014#
--     @layout@ /must/ have been created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDescriptorSetLayoutBindingOffsetEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDescriptorSetLayoutBindingOffsetEXT-layout-parameter#
--     @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSetLayout' handle
--
-- -   #VUID-vkGetDescriptorSetLayoutBindingOffsetEXT-pOffset-parameter#
--     @pOffset@ /must/ be a valid pointer to a
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' value
--
-- -   #VUID-vkGetDescriptorSetLayoutBindingOffsetEXT-layout-parent#
--     @layout@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
getDescriptorSetLayoutBindingOffsetEXT :: forall io
                                        . (MonadIO io)
                                       => -- | @device@ is the logical device that gets the offset.
                                          Device
                                       -> -- | @layout@ is the descriptor set layout being queried.
                                          DescriptorSetLayout
                                       -> -- | @binding@ is the binding number being queried.
                                          ("binding" ::: Word32)
                                       -> io (("offset" ::: DeviceSize))
getDescriptorSetLayoutBindingOffsetEXT device
                                         layout
                                         binding = liftIO . evalContT $ do
  let vkGetDescriptorSetLayoutBindingOffsetEXTPtr = pVkGetDescriptorSetLayoutBindingOffsetEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDescriptorSetLayoutBindingOffsetEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDescriptorSetLayoutBindingOffsetEXT is null" Nothing Nothing
  let vkGetDescriptorSetLayoutBindingOffsetEXT' = mkVkGetDescriptorSetLayoutBindingOffsetEXT vkGetDescriptorSetLayoutBindingOffsetEXTPtr
  pPOffset <- ContT $ bracket (callocBytes @DeviceSize 8) free
  lift $ traceAroundEvent "vkGetDescriptorSetLayoutBindingOffsetEXT" (vkGetDescriptorSetLayoutBindingOffsetEXT'
                                                                        (deviceHandle (device))
                                                                        (layout)
                                                                        (binding)
                                                                        (pPOffset))
  pOffset <- lift $ peek @DeviceSize pPOffset
  pure $ (pOffset)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorEXT
  :: FunPtr (Ptr Device_T -> Ptr DescriptorGetInfoEXT -> CSize -> Ptr () -> IO ()) -> Ptr Device_T -> Ptr DescriptorGetInfoEXT -> CSize -> Ptr () -> IO ()

-- | vkGetDescriptorEXT - To get a descriptor to place in a buffer
--
-- = Description
--
-- The size of the data for each descriptor type is determined by the value
-- in 'PhysicalDeviceDescriptorBufferPropertiesEXT'. This value also
-- defines the stride in bytes for arrays of that descriptor type.
--
-- If the
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT'::@combinedImageSamplerDescriptorSingleArray@
-- property is 'Vulkan.Core10.FundamentalTypes.FALSE' the implementation
-- requires an array of
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
-- descriptors to be written into a descriptor buffer as an array of image
-- descriptors, immediately followed by an array of sampler descriptors.
-- Applications /must/ write the first
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT'::@sampledImageDescriptorSize@
-- bytes of the data returned through @pDescriptor@ to the first array, and
-- the remaining
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT'::@samplerDescriptorSize@
-- bytes of the data to the second array. For variable-sized descriptor
-- bindings of
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
-- descriptors, the two arrays each have a size equal to the upper bound
-- @descriptorCount@ of that binding.
--
-- A descriptor obtained by this command references the underlying
-- 'Vulkan.Core10.Handles.ImageView' or 'Vulkan.Core10.Handles.Sampler',
-- and these objects /must/ not be destroyed before the last time a
-- descriptor is dynamically accessed. For descriptor types which consume
-- an address instead of an object, the underlying
-- 'Vulkan.Core10.Handles.Buffer' is referenced instead.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDescriptorEXT-None-08015# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetDescriptorEXT-dataSize-08125# @dataSize@ /must/ equal the
--     size of a descriptor of type 'DescriptorGetInfoEXT'::@type@
--     determined by the value in
--     'PhysicalDeviceDescriptorBufferPropertiesEXT' , or determined by
--     'PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT'::@combinedImageSamplerDensityMapDescriptorSize@
--     if @pDescriptorInfo@ specifies a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     whose 'Vulkan.Core10.Handles.Sampler' was created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT'
--     set
--
-- -   #VUID-vkGetDescriptorEXT-pDescriptor-08016# @pDescriptor@ /must/ be
--     a valid pointer to an array of at least @dataSize@ bytes
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDescriptorEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDescriptorEXT-pDescriptorInfo-parameter#
--     @pDescriptorInfo@ /must/ be a valid pointer to a valid
--     'DescriptorGetInfoEXT' structure
--
-- -   #VUID-vkGetDescriptorEXT-pDescriptor-parameter# @pDescriptor@ /must/
--     be a valid pointer to an array of @dataSize@ bytes
--
-- -   #VUID-vkGetDescriptorEXT-dataSize-arraylength# @dataSize@ /must/ be
--     greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'DescriptorGetInfoEXT', 'Vulkan.Core10.Handles.Device'
getDescriptorEXT :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that gets the descriptor.
                    Device
                 -> -- | @pDescriptorInfo@ is a pointer to a 'DescriptorGetInfoEXT' structure
                    -- specifying the parameters of the descriptor to get.
                    ("descriptorInfo" ::: DescriptorGetInfoEXT)
                 -> -- | @dataSize@ is the amount of the descriptor data to get in bytes.
                    ("dataSize" ::: Word64)
                 -> -- | @pDescriptor@ is a pointer to a user-allocated buffer where the
                    -- descriptor will be written.
                    ("descriptor" ::: Ptr ())
                 -> io ()
getDescriptorEXT device
                   descriptorInfo
                   dataSize
                   descriptor = liftIO . evalContT $ do
  let vkGetDescriptorEXTPtr = pVkGetDescriptorEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDescriptorEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDescriptorEXT is null" Nothing Nothing
  let vkGetDescriptorEXT' = mkVkGetDescriptorEXT vkGetDescriptorEXTPtr
  pDescriptorInfo <- ContT $ withCStruct (descriptorInfo)
  lift $ traceAroundEvent "vkGetDescriptorEXT" (vkGetDescriptorEXT'
                                                  (deviceHandle (device))
                                                  pDescriptorInfo
                                                  (CSize (dataSize))
                                                  (descriptor))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorBuffersEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr (SomeStruct DescriptorBufferBindingInfoEXT) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr (SomeStruct DescriptorBufferBindingInfoEXT) -> IO ()

-- | vkCmdBindDescriptorBuffersEXT - Binding descriptor buffers to a command
-- buffer
--
-- = Description
--
-- 'cmdBindDescriptorBuffersEXT' causes any offsets previously set by
-- 'cmdSetDescriptorBufferOffsetsEXT' that use the bindings numbered [@0@..
-- @bufferCount@-1] to be no longer valid for subsequent bound pipeline
-- commands. Any previously bound buffers at binding points greater than or
-- equal to @bufferCount@ are unbound.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-None-08047# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-maxSamplerDescriptorBufferBindings-08048#
--     There /must/ be no more than
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxSamplerDescriptorBufferBindings@
--     descriptor buffers containing sampler descriptor data bound
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-maxResourceDescriptorBufferBindings-08049#
--     There /must/ be no more than
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxResourceDescriptorBufferBindings@
--     descriptor buffers containing resource descriptor data bound
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-None-08050# There /must/ be no
--     more than @1@ descriptor buffer bound that was created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-bufferCount-08051# @bufferCount@
--     /must/ be less than or equal to
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxDescriptorBufferBindings@
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-pBindingInfos-08052# For any
--     element of @pBindingInfos@, if the buffer from which @address@ was
--     queried is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-pBindingInfos-08053# For any
--     element of @pBindingInfos@, the buffer from which @address@ was
--     queried /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT'
--     bit set if it contains sampler descriptor data
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-pBindingInfos-08054# For any
--     element of @pBindingInfos@, the buffer from which @address@ was
--     queried /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'
--     bit set if it contains resource descriptor data
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-pBindingInfos-08055# For any
--     element of @pBindingInfos@, @usage@ /must/ match the buffer from
--     which @address@ was queried
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-pBindingInfos-parameter#
--     @pBindingInfos@ /must/ be a valid pointer to an array of
--     @bufferCount@ valid 'DescriptorBufferBindingInfoEXT' structures
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdBindDescriptorBuffersEXT-bufferCount-arraylength#
--     @bufferCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DescriptorBufferBindingInfoEXT'
cmdBindDescriptorBuffersEXT :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer that the descriptor buffers will
                               -- be bound to.
                               CommandBuffer
                            -> -- | @pBindingInfos@ is a pointer to an array of
                               -- 'DescriptorBufferBindingInfoEXT' structures.
                               ("bindingInfos" ::: Vector (SomeStruct DescriptorBufferBindingInfoEXT))
                            -> io ()
cmdBindDescriptorBuffersEXT commandBuffer bindingInfos = liftIO . evalContT $ do
  let vkCmdBindDescriptorBuffersEXTPtr = pVkCmdBindDescriptorBuffersEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindDescriptorBuffersEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindDescriptorBuffersEXT is null" Nothing Nothing
  let vkCmdBindDescriptorBuffersEXT' = mkVkCmdBindDescriptorBuffersEXT vkCmdBindDescriptorBuffersEXTPtr
  pPBindingInfos <- ContT $ allocaBytes @(DescriptorBufferBindingInfoEXT _) ((Data.Vector.length (bindingInfos)) * 32)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPBindingInfos `plusPtr` (32 * (i)) :: Ptr (DescriptorBufferBindingInfoEXT _))) (e) . ($ ())) (bindingInfos)
  lift $ traceAroundEvent "vkCmdBindDescriptorBuffersEXT" (vkCmdBindDescriptorBuffersEXT'
                                                             (commandBufferHandle (commandBuffer))
                                                             ((fromIntegral (Data.Vector.length $ (bindingInfos)) :: Word32))
                                                             (forgetExtensions (pPBindingInfos)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDescriptorBufferOffsetsEXT
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr Word32 -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr Word32 -> Ptr DeviceSize -> IO ()

-- | vkCmdSetDescriptorBufferOffsetsEXT - Setting descriptor buffer offsets
-- in a command buffer
--
-- = Description
--
-- 'cmdSetDescriptorBufferOffsetsEXT' binds @setCount@ pairs of descriptor
-- buffers, specified by indices into the binding points bound using
-- 'cmdBindDescriptorBuffersEXT', and buffer offsets to set numbers
-- [@firstSet@..@firstSet@+@descriptorSetCount@-1] for subsequent
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-bindpoint-commands bound pipeline commands>
-- set by @pipelineBindPoint@. Set [@firstSet@ + i] is bound to the
-- descriptor buffer at binding @pBufferIndices@[i] at an offset of
-- @pOffsets@[i]. Any bindings that were previously applied via these sets,
-- or calls to 'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- are no longer valid. Other sets will also be invalidated upon calling
-- this command if @layout@ differs from the pipeline layout used to bind
-- those other sets, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
--
-- After binding descriptors, applications /can/ modify descriptor memory
-- either by performing writes on the host or with device commands. When
-- descriptor memory is updated with device commands, visibility for the
-- shader stage accessing a descriptor is ensured with the
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT'
-- access flag. Implementations /must/ not access resources referenced by
-- these descriptors unless they are dynamically accessed by shaders.
-- Descriptors bound with this call /can/ be undefined if they are not
-- dynamically accessed by shaders.
--
-- Implementations /may/ read descriptor data for any statically accessed
-- descriptor if the @binding@ in @layout@ is not declared with the
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
-- flag. If the @binding@ in @layout@ is declared with
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT',
-- implementations /must/ not read descriptor data that is not dynamically
-- accessed.
--
-- Applications /must/ ensure that any descriptor which the implementation
-- /may/ read /must/ be in-bounds of the underlying descriptor buffer
-- binding.
--
-- Note
--
-- Applications can freely decide how large a variable descriptor buffer
-- binding is, so it may not be safe to read such descriptor payloads
-- statically. The intention of these rules is to allow implementations to
-- speculatively prefetch descriptor payloads where feasible.
--
-- Dynamically accessing a resource through descriptor data from an unbound
-- region of a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#sparsememory-partially-resident-buffers sparse partially-resident buffer>
-- will result in invalid descriptor data being read, and therefore
-- undefined behavior.
--
-- Note
--
-- For descriptors written by the host, visibility is implied through the
-- automatic visibility operation on queue submit, and there is no need to
-- consider @VK_ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT@. Explicit
-- synchronization for descriptors is only required when descriptors are
-- updated on the device.
--
-- Note
--
-- The requirements above imply that all descriptor bindings have been
-- defined with the equivalent of
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT',
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT'
-- and
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT',
-- but enabling those features is not required to get this behavior.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-None-08060# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pOffsets-08061# The offsets
--     in @pOffsets@ /must/ be aligned to
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@descriptorBufferOffsetAlignment@
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pOffsets-08063# The offsets
--     in @pOffsets@ /must/ be small enough such that any descriptor
--     binding referenced by @layout@ without the
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
--     flag computes a valid address inside the underlying
--     'Vulkan.Core10.Handles.Buffer'
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pOffsets-08126# The offsets
--     in @pOffsets@ /must/ be small enough such that any location accessed
--     by a shader as a sampler descriptor /must/ be within
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxSamplerDescriptorBufferRange@
--     of the sampler descriptor buffer binding
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pOffsets-08127# The offsets
--     in @pOffsets@ /must/ be small enough such that any location accessed
--     by a shader as a resource descriptor /must/ be within
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxResourceDescriptorBufferRange@
--     of the resource descriptor buffer binding
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pBufferIndices-08064# Each
--     element of @pBufferIndices@ /must/ be less than
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxDescriptorBufferBindings@
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pBufferIndices-08065# Each
--     element of @pBufferIndices@ /must/ reference a valid descriptor
--     buffer binding set by a previous call to
--     'cmdBindDescriptorBuffersEXT' in @commandBuffer@
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-firstSet-08066# The sum of
--     @firstSet@ and @setCount@ /must/ be less than or equal to
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pipelineBindPoint-08067#
--     @pipelineBindPoint@ /must/ be supported by the @commandBuffer@’s
--     parent 'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-firstSet-09006# The
--     'Vulkan.Core10.Handles.DescriptorSetLayout' for each set from
--     @firstSet@ to @firstSet@ + @setCount@ when @layout@ was created
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--     bit set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-layout-parameter# @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pBufferIndices-parameter#
--     @pBufferIndices@ /must/ be a valid pointer to an array of @setCount@
--     @uint32_t@ values
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-pOffsets-parameter#
--     @pOffsets@ /must/ be a valid pointer to an array of @setCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-setCount-arraylength#
--     @setCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsetsEXT-commonparent# Both of
--     @commandBuffer@, and @layout@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Handles.PipelineLayout'
cmdSetDescriptorBufferOffsetsEXT :: forall io
                                  . (MonadIO io)
                                 => -- | @commandBuffer@ is the command buffer in which the descriptor buffer
                                    -- offsets will be set.
                                    CommandBuffer
                                 -> -- | @pipelineBindPoint@ is a
                                    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' indicating the
                                    -- type of the pipeline that will use the descriptors.
                                    PipelineBindPoint
                                 -> -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
                                    -- program the bindings.
                                    PipelineLayout
                                 -> -- | @firstSet@ is the number of the first set to be bound.
                                    ("firstSet" ::: Word32)
                                 -> -- | @pBufferIndices@ is a pointer to an array of indices into the descriptor
                                    -- buffer binding points set by 'cmdBindDescriptorBuffersEXT'.
                                    ("bufferIndices" ::: Vector Word32)
                                 -> -- | @pOffsets@ is a pointer to an array of
                                    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' offsets to apply to the
                                    -- bound descriptor buffers.
                                    ("offsets" ::: Vector DeviceSize)
                                 -> io ()
cmdSetDescriptorBufferOffsetsEXT commandBuffer
                                   pipelineBindPoint
                                   layout
                                   firstSet
                                   bufferIndices
                                   offsets = liftIO . evalContT $ do
  let vkCmdSetDescriptorBufferOffsetsEXTPtr = pVkCmdSetDescriptorBufferOffsetsEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetDescriptorBufferOffsetsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDescriptorBufferOffsetsEXT is null" Nothing Nothing
  let vkCmdSetDescriptorBufferOffsetsEXT' = mkVkCmdSetDescriptorBufferOffsetsEXT vkCmdSetDescriptorBufferOffsetsEXTPtr
  let pBufferIndicesLength = Data.Vector.length $ (bufferIndices)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBufferIndicesLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBufferIndices must have the same length" Nothing Nothing
  pPBufferIndices <- ContT $ allocaBytes @Word32 ((Data.Vector.length (bufferIndices)) * 4)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferIndices `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (bufferIndices)
  pPOffsets <- ContT $ allocaBytes @DeviceSize ((Data.Vector.length (offsets)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  lift $ traceAroundEvent "vkCmdSetDescriptorBufferOffsetsEXT" (vkCmdSetDescriptorBufferOffsetsEXT'
                                                                  (commandBufferHandle (commandBuffer))
                                                                  (pipelineBindPoint)
                                                                  (layout)
                                                                  (firstSet)
                                                                  ((fromIntegral pBufferIndicesLength :: Word32))
                                                                  (pPBufferIndices)
                                                                  (pPOffsets))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorBufferEmbeddedSamplersEXT
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> IO ()

-- | vkCmdBindDescriptorBufferEmbeddedSamplersEXT - Setting embedded
-- immutable samplers offsets in a command buffer
--
-- = Description
--
-- 'cmdBindDescriptorBufferEmbeddedSamplersEXT' binds the embedded
-- immutable samplers in @set@ of @layout@ to @set@ for the command buffer
-- for subsequent
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-bindpoint-commands bound pipeline commands>
-- set by @pipelineBindPoint@. Any previous binding to this set by
-- 'cmdSetDescriptorBufferOffsetsEXT' or this command is overwritten. Any
-- sets that were last bound by a call to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets' are
-- invalidated upon calling this command. Other sets will also be
-- invalidated upon calling this command if @layout@ differs from the
-- pipeline layout used to bind those other sets, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-None-08068# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-pipelineBindPoint-08069#
--     @pipelineBindPoint@ /must/ be supported by the @commandBuffer@’s
--     parent 'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-set-08070# The
--     'Vulkan.Core10.Handles.DescriptorSetLayout' at index @set@ when
--     @layout@ was created /must/ have been created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_EMBEDDED_IMMUTABLE_SAMPLERS_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-set-08071# @set@
--     /must/ be less than or equal to
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-layout-parameter#
--     @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-videocoding# This
--     command /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplersEXT-commonparent#
--     Both of @commandBuffer@, and @layout@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Handles.PipelineLayout'
cmdBindDescriptorBufferEmbeddedSamplersEXT :: forall io
                                            . (MonadIO io)
                                           => -- | @commandBuffer@ is the command buffer that the embedded immutable
                                              -- samplers will be bound to.
                                              CommandBuffer
                                           -> -- | @pipelineBindPoint@ is a
                                              -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' indicating the
                                              -- type of the pipeline that will use the embedded immutable samplers.
                                              PipelineBindPoint
                                           -> -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
                                              -- program the bindings.
                                              PipelineLayout
                                           -> -- | @set@ is the number of the set to be bound.
                                              ("set" ::: Word32)
                                           -> io ()
cmdBindDescriptorBufferEmbeddedSamplersEXT commandBuffer
                                             pipelineBindPoint
                                             layout
                                             set = liftIO $ do
  let vkCmdBindDescriptorBufferEmbeddedSamplersEXTPtr = pVkCmdBindDescriptorBufferEmbeddedSamplersEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdBindDescriptorBufferEmbeddedSamplersEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindDescriptorBufferEmbeddedSamplersEXT is null" Nothing Nothing
  let vkCmdBindDescriptorBufferEmbeddedSamplersEXT' = mkVkCmdBindDescriptorBufferEmbeddedSamplersEXT vkCmdBindDescriptorBufferEmbeddedSamplersEXTPtr
  traceAroundEvent "vkCmdBindDescriptorBufferEmbeddedSamplersEXT" (vkCmdBindDescriptorBufferEmbeddedSamplersEXT'
                                                                     (commandBufferHandle (commandBuffer))
                                                                     (pipelineBindPoint)
                                                                     (layout)
                                                                     (set))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferOpaqueCaptureDescriptorDataEXT
  :: FunPtr (Ptr Device_T -> Ptr BufferCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr BufferCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result

-- | vkGetBufferOpaqueCaptureDescriptorDataEXT - Get buffer opaque capture
-- descriptor data
--
-- == Valid Usage
--
-- -   #VUID-vkGetBufferOpaqueCaptureDescriptorDataEXT-None-08072# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBufferCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetBufferOpaqueCaptureDescriptorDataEXT-pData-08073# @pData@
--     /must/ point to a buffer that is at least
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@bufferCaptureReplayDescriptorDataSize@
--     bytes in size
--
-- -   #VUID-vkGetBufferOpaqueCaptureDescriptorDataEXT-device-08074# If
--     @device@ was created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetBufferOpaqueCaptureDescriptorDataEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetBufferOpaqueCaptureDescriptorDataEXT-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'BufferCaptureDescriptorDataInfoEXT' structure
--
-- -   #VUID-vkGetBufferOpaqueCaptureDescriptorDataEXT-pData-parameter#
--     @pData@ /must/ be a pointer value
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'BufferCaptureDescriptorDataInfoEXT', 'Vulkan.Core10.Handles.Device'
getBufferOpaqueCaptureDescriptorDataEXT :: forall io
                                         . (MonadIO io)
                                        => -- | @device@ is the logical device that gets the data.
                                           Device
                                        -> -- | @pInfo@ is a pointer to a 'BufferCaptureDescriptorDataInfoEXT' structure
                                           -- specifying the buffer.
                                           BufferCaptureDescriptorDataInfoEXT
                                        -> -- | @pData@ is a pointer to a user-allocated buffer where the data will be
                                           -- written.
                                           ("data" ::: Ptr ())
                                        -> io ()
getBufferOpaqueCaptureDescriptorDataEXT device
                                          info
                                          data' = liftIO . evalContT $ do
  let vkGetBufferOpaqueCaptureDescriptorDataEXTPtr = pVkGetBufferOpaqueCaptureDescriptorDataEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetBufferOpaqueCaptureDescriptorDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetBufferOpaqueCaptureDescriptorDataEXT is null" Nothing Nothing
  let vkGetBufferOpaqueCaptureDescriptorDataEXT' = mkVkGetBufferOpaqueCaptureDescriptorDataEXT vkGetBufferOpaqueCaptureDescriptorDataEXTPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetBufferOpaqueCaptureDescriptorDataEXT" (vkGetBufferOpaqueCaptureDescriptorDataEXT'
                                                                              (deviceHandle (device))
                                                                              pInfo
                                                                              (data'))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageOpaqueCaptureDescriptorDataEXT
  :: FunPtr (Ptr Device_T -> Ptr ImageCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr ImageCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result

-- | vkGetImageOpaqueCaptureDescriptorDataEXT - Get image opaque capture
-- descriptor data
--
-- == Valid Usage
--
-- -   #VUID-vkGetImageOpaqueCaptureDescriptorDataEXT-None-08076# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBufferCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetImageOpaqueCaptureDescriptorDataEXT-pData-08077# @pData@
--     /must/ point to a buffer that is at least
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@imageCaptureReplayDescriptorDataSize@
--     bytes in size
--
-- -   #VUID-vkGetImageOpaqueCaptureDescriptorDataEXT-device-08078# If
--     @device@ was created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetImageOpaqueCaptureDescriptorDataEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetImageOpaqueCaptureDescriptorDataEXT-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'ImageCaptureDescriptorDataInfoEXT' structure
--
-- -   #VUID-vkGetImageOpaqueCaptureDescriptorDataEXT-pData-parameter#
--     @pData@ /must/ be a pointer value
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Device', 'ImageCaptureDescriptorDataInfoEXT'
getImageOpaqueCaptureDescriptorDataEXT :: forall io
                                        . (MonadIO io)
                                       => -- | @device@ is the logical device that gets the data.
                                          Device
                                       -> -- | @pInfo@ is a pointer to a 'ImageCaptureDescriptorDataInfoEXT' structure
                                          -- specifying the image.
                                          ImageCaptureDescriptorDataInfoEXT
                                       -> -- | @pData@ is a pointer to a user-allocated buffer where the data will be
                                          -- written.
                                          ("data" ::: Ptr ())
                                       -> io ()
getImageOpaqueCaptureDescriptorDataEXT device
                                         info
                                         data' = liftIO . evalContT $ do
  let vkGetImageOpaqueCaptureDescriptorDataEXTPtr = pVkGetImageOpaqueCaptureDescriptorDataEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageOpaqueCaptureDescriptorDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageOpaqueCaptureDescriptorDataEXT is null" Nothing Nothing
  let vkGetImageOpaqueCaptureDescriptorDataEXT' = mkVkGetImageOpaqueCaptureDescriptorDataEXT vkGetImageOpaqueCaptureDescriptorDataEXTPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetImageOpaqueCaptureDescriptorDataEXT" (vkGetImageOpaqueCaptureDescriptorDataEXT'
                                                                             (deviceHandle (device))
                                                                             pInfo
                                                                             (data'))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewOpaqueCaptureDescriptorDataEXT
  :: FunPtr (Ptr Device_T -> Ptr ImageViewCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr ImageViewCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result

-- | vkGetImageViewOpaqueCaptureDescriptorDataEXT - Get image view opaque
-- capture descriptor data
--
-- == Valid Usage
--
-- -   #VUID-vkGetImageViewOpaqueCaptureDescriptorDataEXT-None-08080# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBufferCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetImageViewOpaqueCaptureDescriptorDataEXT-pData-08081#
--     @pData@ /must/ point to a buffer that is at least
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@imageViewCaptureReplayDescriptorDataSize@
--     bytes in size
--
-- -   #VUID-vkGetImageViewOpaqueCaptureDescriptorDataEXT-device-08082# If
--     @device@ was created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetImageViewOpaqueCaptureDescriptorDataEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetImageViewOpaqueCaptureDescriptorDataEXT-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'ImageViewCaptureDescriptorDataInfoEXT' structure
--
-- -   #VUID-vkGetImageViewOpaqueCaptureDescriptorDataEXT-pData-parameter#
--     @pData@ /must/ be a pointer value
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Device', 'ImageViewCaptureDescriptorDataInfoEXT'
getImageViewOpaqueCaptureDescriptorDataEXT :: forall io
                                            . (MonadIO io)
                                           => -- | @device@ is the logical device that gets the data.
                                              Device
                                           -> -- | @pInfo@ is a pointer to a 'ImageViewCaptureDescriptorDataInfoEXT'
                                              -- structure specifying the image view.
                                              ImageViewCaptureDescriptorDataInfoEXT
                                           -> -- | @pData@ is a pointer to a user-allocated buffer where the data will be
                                              -- written.
                                              ("data" ::: Ptr ())
                                           -> io ()
getImageViewOpaqueCaptureDescriptorDataEXT device
                                             info
                                             data' = liftIO . evalContT $ do
  let vkGetImageViewOpaqueCaptureDescriptorDataEXTPtr = pVkGetImageViewOpaqueCaptureDescriptorDataEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageViewOpaqueCaptureDescriptorDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageViewOpaqueCaptureDescriptorDataEXT is null" Nothing Nothing
  let vkGetImageViewOpaqueCaptureDescriptorDataEXT' = mkVkGetImageViewOpaqueCaptureDescriptorDataEXT vkGetImageViewOpaqueCaptureDescriptorDataEXTPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetImageViewOpaqueCaptureDescriptorDataEXT" (vkGetImageViewOpaqueCaptureDescriptorDataEXT'
                                                                                 (deviceHandle (device))
                                                                                 pInfo
                                                                                 (data'))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSamplerOpaqueCaptureDescriptorDataEXT
  :: FunPtr (Ptr Device_T -> Ptr SamplerCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr SamplerCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result

-- | vkGetSamplerOpaqueCaptureDescriptorDataEXT - Get sampler opaque capture
-- descriptor data
--
-- == Valid Usage
--
-- -   #VUID-vkGetSamplerOpaqueCaptureDescriptorDataEXT-None-08084# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBufferCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetSamplerOpaqueCaptureDescriptorDataEXT-pData-08085#
--     @pData@ /must/ point to a buffer that is at least
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@samplerCaptureReplayDescriptorDataSize@
--     bytes in size
--
-- -   #VUID-vkGetSamplerOpaqueCaptureDescriptorDataEXT-device-08086# If
--     @device@ was created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetSamplerOpaqueCaptureDescriptorDataEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetSamplerOpaqueCaptureDescriptorDataEXT-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'SamplerCaptureDescriptorDataInfoEXT' structure
--
-- -   #VUID-vkGetSamplerOpaqueCaptureDescriptorDataEXT-pData-parameter#
--     @pData@ /must/ be a pointer value
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Device', 'SamplerCaptureDescriptorDataInfoEXT'
getSamplerOpaqueCaptureDescriptorDataEXT :: forall io
                                          . (MonadIO io)
                                         => -- | @device@ is the logical device that gets the data.
                                            Device
                                         -> -- | @pInfo@ is a pointer to a 'SamplerCaptureDescriptorDataInfoEXT'
                                            -- structure specifying the sampler.
                                            SamplerCaptureDescriptorDataInfoEXT
                                         -> -- | @pData@ is a pointer to a user-allocated buffer where the data will be
                                            -- written.
                                            ("data" ::: Ptr ())
                                         -> io ()
getSamplerOpaqueCaptureDescriptorDataEXT device
                                           info
                                           data' = liftIO . evalContT $ do
  let vkGetSamplerOpaqueCaptureDescriptorDataEXTPtr = pVkGetSamplerOpaqueCaptureDescriptorDataEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetSamplerOpaqueCaptureDescriptorDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSamplerOpaqueCaptureDescriptorDataEXT is null" Nothing Nothing
  let vkGetSamplerOpaqueCaptureDescriptorDataEXT' = mkVkGetSamplerOpaqueCaptureDescriptorDataEXT vkGetSamplerOpaqueCaptureDescriptorDataEXTPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetSamplerOpaqueCaptureDescriptorDataEXT" (vkGetSamplerOpaqueCaptureDescriptorDataEXT'
                                                                               (deviceHandle (device))
                                                                               pInfo
                                                                               (data'))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr AccelerationStructureCaptureDescriptorDataInfoEXT -> Ptr () -> IO Result

-- | vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT - Get
-- acceleration structure opaque capture descriptor data
--
-- == Valid Usage
--
-- -   #VUID-vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT-None-08088#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBufferCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT-pData-08089#
--     @pData@ /must/ point to a buffer that is at least
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@accelerationStructureCaptureReplayDescriptorDataSize@
--     bytes in size
--
-- -   #VUID-vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT-device-08090#
--     If @device@ was created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureCaptureDescriptorDataInfoEXT' structure
--
-- -   #VUID-vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT-pData-parameter#
--     @pData@ /must/ be a pointer value
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>,
-- 'AccelerationStructureCaptureDescriptorDataInfoEXT',
-- 'Vulkan.Core10.Handles.Device'
getAccelerationStructureOpaqueCaptureDescriptorDataEXT :: forall io
                                                        . (MonadIO io)
                                                       => -- | @device@ is the logical device that gets the data.
                                                          Device
                                                       -> -- | @pInfo@ is a pointer to a
                                                          -- 'AccelerationStructureCaptureDescriptorDataInfoEXT' structure specifying
                                                          -- the acceleration structure.
                                                          AccelerationStructureCaptureDescriptorDataInfoEXT
                                                       -> -- | @pData@ is a pointer to a user-allocated buffer where the data will be
                                                          -- written.
                                                          ("data" ::: Ptr ())
                                                       -> io ()
getAccelerationStructureOpaqueCaptureDescriptorDataEXT device
                                                         info
                                                         data' = liftIO . evalContT $ do
  let vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXTPtr = pVkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT is null" Nothing Nothing
  let vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT' = mkVkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXTPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT" (vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT'
                                                                                             (deviceHandle (device))
                                                                                             pInfo
                                                                                             (data'))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkPhysicalDeviceDescriptorBufferFeaturesEXT - Structure describing the
-- descriptor buffer features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorBufferFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDescriptorBufferFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorBufferFeaturesEXT = PhysicalDeviceDescriptorBufferFeaturesEXT
  { -- | #features-descriptorBuffer# @descriptorBuffer@ indicates that the
    -- implementation supports putting shader-accessible descriptors directly
    -- in memory.
    descriptorBuffer :: Bool
  , -- | #features-descriptorBufferCaptureReplay# @descriptorBufferCaptureReplay@
    -- indicates that the implementation supports capture and replay when using
    -- descriptor buffers. If this is 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- all resources created with
    -- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT',
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT',
    -- 'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT',
    -- 'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT',
    -- or
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
    -- /must/ be created before resources of the same types without those
    -- flags.
    descriptorBufferCaptureReplay :: Bool
  , -- | #features-descriptorBufferImageLayoutIgnored#
    -- @descriptorBufferImageLayoutIgnored@ indicates that the implementation
    -- will ignore @imageLayout@ in
    -- 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo' when calling
    -- 'getDescriptorEXT'.
    descriptorBufferImageLayoutIgnored :: Bool
  , -- | #features-descriptorBufferPushDescriptors#
    -- @descriptorBufferPushDescriptors@ indicates that the implementation
    -- supports using push descriptors with descriptor buffers.
    descriptorBufferPushDescriptors :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorBufferFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDescriptorBufferFeaturesEXT

instance ToCStruct PhysicalDeviceDescriptorBufferFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorBufferFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (descriptorBuffer))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (descriptorBufferCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (descriptorBufferImageLayoutIgnored))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (descriptorBufferPushDescriptors))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorBufferFeaturesEXT where
  peekCStruct p = do
    descriptorBuffer <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    descriptorBufferCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    descriptorBufferImageLayoutIgnored <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    descriptorBufferPushDescriptors <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceDescriptorBufferFeaturesEXT
             (bool32ToBool descriptorBuffer)
             (bool32ToBool descriptorBufferCaptureReplay)
             (bool32ToBool descriptorBufferImageLayoutIgnored)
             (bool32ToBool descriptorBufferPushDescriptors)

instance Storable PhysicalDeviceDescriptorBufferFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorBufferFeaturesEXT where
  zero = PhysicalDeviceDescriptorBufferFeaturesEXT
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceDescriptorBufferPropertiesEXT - Structure describing
-- descriptor buffer properties supported by an implementation
--
-- = Description
--
-- A descriptor binding with type
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.DESCRIPTOR_TYPE_MUTABLE_VALVE'
-- has a descriptor size which is implied by the descriptor types included
-- in the
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'::@pDescriptorTypes@
-- list. The descriptor size is equal to the maximum size of any descriptor
-- type included in the @pDescriptorTypes@ list.
--
-- As there is no way to request robust and non-robust descriptors
-- separately, or specify robust\/non-robust descriptors in the set layout,
-- if
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
-- is enabled then robust descriptors are always used.
--
-- If the 'PhysicalDeviceDescriptorBufferPropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorBufferPropertiesEXT = PhysicalDeviceDescriptorBufferPropertiesEXT
  { -- | #limits-combinedImageSamplerDescriptorSingleArray#
    -- @combinedImageSamplerDescriptorSingleArray@ indicates that the
    -- implementation does not require an array of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
    -- descriptors to be written into a descriptor buffer as an array of image
    -- descriptors, immediately followed by an array of sampler descriptors.
    combinedImageSamplerDescriptorSingleArray :: Bool
  , -- | #limits-bufferlessPushDescriptors# @bufferlessPushDescriptors@ indicates
    -- that the implementation does not require a buffer created with
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT'
    -- to be bound when using push descriptors.
    bufferlessPushDescriptors :: Bool
  , -- | #limits-allowSamplerImageViewPostSubmitCreation#
    -- @allowSamplerImageViewPostSubmitCreation@ indicates that the
    -- implementation does not restrict when the
    -- 'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
    -- objects used to retrieve descriptor data /can/ be created in relation to
    -- command buffer submission. If this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', then the application /must/
    -- create any 'Vulkan.Core10.Handles.Sampler' or
    -- 'Vulkan.Core10.Handles.ImageView' objects whose descriptor data is
    -- accessed during the execution of a command buffer, before the
    -- 'Vulkan.Core10.Queue.queueSubmit' , or
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.queueSubmit2', call
    -- that submits that command buffer.
    allowSamplerImageViewPostSubmitCreation :: Bool
  , -- | #limits-descriptorBufferOffsetAlignment#
    -- @descriptorBufferOffsetAlignment@ indicates the /required/ alignment in
    -- bytes when setting offsets into the descriptor buffer.
    descriptorBufferOffsetAlignment :: DeviceSize
  , -- | #limits-maxDescriptorBufferBindings# @maxDescriptorBufferBindings@
    -- indicates the maximum sum total number of descriptor buffers and
    -- embedded immutable sampler sets that /can/ be bound.
    maxDescriptorBufferBindings :: Word32
  , -- | #limits-maxResourceDescriptorBufferBindings#
    -- @maxResourceDescriptorBufferBindings@ indicates the maximum number of
    -- resource descriptor buffers that /can/ be bound.
    maxResourceDescriptorBufferBindings :: Word32
  , -- | #limits-maxSamplerDescriptorBufferBindings#
    -- @maxSamplerDescriptorBufferBindings@ indicates the maximum number of
    -- sampler descriptor buffers that /can/ be bound.
    maxSamplerDescriptorBufferBindings :: Word32
  , -- | #limits-maxEmbeddedImmutableSamplerBindings#
    -- @maxEmbeddedImmutableSamplerBindings@ indicates the maximum number of
    -- embedded immutable sampler sets that /can/ be bound.
    maxEmbeddedImmutableSamplerBindings :: Word32
  , -- | #limits-maxEmbeddedImmutableSamplers# @maxEmbeddedImmutableSamplers@
    -- indicates the maximum number of unique immutable samplers in descriptor
    -- set layouts created with
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_EMBEDDED_IMMUTABLE_SAMPLERS_BIT_EXT',
    -- and pipeline layouts created from them, which /can/ simultaneously exist
    -- on a device.
    maxEmbeddedImmutableSamplers :: Word32
  , -- | #limits-bufferCaptureReplayDescriptorDataSize#
    -- @bufferCaptureReplayDescriptorDataSize@ indicates the maximum size in
    -- bytes of the opaque data used for capture and replay with buffers.
    bufferCaptureReplayDescriptorDataSize :: Word64
  , -- | #limits-imageCaptureReplayDescriptorDataSize#
    -- @imageCaptureReplayDescriptorDataSize@ indicates the maximum size in
    -- bytes of the opaque data used for capture and replay with images.
    imageCaptureReplayDescriptorDataSize :: Word64
  , -- | #limits-imageViewCaptureReplayDescriptorDataSize#
    -- @imageViewCaptureReplayDescriptorDataSize@ indicates the maximum size in
    -- bytes of the opaque data used for capture and replay with image views.
    imageViewCaptureReplayDescriptorDataSize :: Word64
  , -- | #limits-samplerCaptureReplayDescriptorDataSize#
    -- @samplerCaptureReplayDescriptorDataSize@ indicates the maximum size in
    -- bytes of the opaque data used for capture and replay with samplers.
    samplerCaptureReplayDescriptorDataSize :: Word64
  , -- | #limits-accelerationStructureCaptureReplayDescriptorDataSize#
    -- @accelerationStructureCaptureReplayDescriptorDataSize@ indicates the
    -- maximum size in bytes of the opaque data used for capture and replay
    -- with acceleration structures.
    accelerationStructureCaptureReplayDescriptorDataSize :: Word64
  , -- | #limits-samplerDescriptorSize# @samplerDescriptorSize@ indicates the
    -- size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER' descriptor.
    samplerDescriptorSize :: Word64
  , -- | #limits-combinedImageSamplerDescriptorSize#
    -- @combinedImageSamplerDescriptorSize@ indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
    -- descriptor.
    combinedImageSamplerDescriptorSize :: Word64
  , -- | #limits-sampledImageDescriptorSize# @sampledImageDescriptorSize@
    -- indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
    -- descriptor.
    sampledImageDescriptorSize :: Word64
  , -- | #limits-storageImageDescriptorSize# @storageImageDescriptorSize@
    -- indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
    -- descriptor.
    storageImageDescriptorSize :: Word64
  , -- | #limits-uniformTexelBufferDescriptorSize#
    -- @uniformTexelBufferDescriptorSize@ indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    -- descriptor if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- feature is not enabled.
    uniformTexelBufferDescriptorSize :: Word64
  , -- | #limits-robustUniformTexelBufferDescriptorSize#
    -- @robustUniformTexelBufferDescriptorSize@ indicates the size in bytes of
    -- a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    -- descriptor if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- feature is enabled.
    robustUniformTexelBufferDescriptorSize :: Word64
  , -- | #limits-storageTexelBufferDescriptorSize#
    -- @storageTexelBufferDescriptorSize@ indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    -- descriptor if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- feature is not enabled.
    storageTexelBufferDescriptorSize :: Word64
  , -- | #limits-robustStorageTexelBufferDescriptorSize#
    -- @robustStorageTexelBufferDescriptorSize@ indicates the size in bytes of
    -- a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    -- descriptor if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- feature is enabled.
    robustStorageTexelBufferDescriptorSize :: Word64
  , -- | #limits-uniformBufferDescriptorSize# @uniformBufferDescriptorSize@
    -- indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
    -- descriptor.
    uniformBufferDescriptorSize :: Word64
  , -- | #limits-robustUniformBufferDescriptorSize#
    -- @robustUniformBufferDescriptorSize@ indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
    -- descriptor if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- feature is enabled.
    robustUniformBufferDescriptorSize :: Word64
  , -- | #limits-storageBufferDescriptorSize# @storageBufferDescriptorSize@
    -- indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
    -- descriptor.
    storageBufferDescriptorSize :: Word64
  , -- | #limits-robustStorageBufferDescriptorSize#
    -- @robustStorageBufferDescriptorSize@ indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
    -- descriptor if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- feature is enabled.
    robustStorageBufferDescriptorSize :: Word64
  , -- | #limits-inputAttachmentDescriptorSize# @inputAttachmentDescriptorSize@
    -- indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
    -- descriptor.
    inputAttachmentDescriptorSize :: Word64
  , -- | #limits-accelerationStructureDescriptorSize#
    -- @accelerationStructureDescriptorSize@ indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR'
    -- or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV'
    -- descriptor.
    accelerationStructureDescriptorSize :: Word64
  , -- | #limits-maxSamplerDescriptorBufferRange#
    -- @maxSamplerDescriptorBufferRange@ indicates the maximum range in bytes
    -- from the address of a sampler descriptor buffer binding that is
    -- accessible to a shader.
    maxSamplerDescriptorBufferRange :: DeviceSize
  , -- | #limits-maxResourceDescriptorBufferRange#
    -- @maxResourceDescriptorBufferRange@ indicates the maximum range in bytes
    -- from the address of a resource descriptor buffer binding that is
    -- accessible to a shader.
    maxResourceDescriptorBufferRange :: DeviceSize
  , -- | #limits-samplerDescriptorBufferAddressSpaceSize#
    -- @samplerDescriptorBufferAddressSpaceSize@ indicates the total size in
    -- bytes of the address space available for descriptor buffers created with
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT'.
    samplerDescriptorBufferAddressSpaceSize :: DeviceSize
  , -- | #limits-resourceDescriptorBufferAddressSpaceSize#
    -- @resourceDescriptorBufferAddressSpaceSize@ indicates the total size in
    -- bytes of the address space available for descriptor buffers created with
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'.
    resourceDescriptorBufferAddressSpaceSize :: DeviceSize
  , -- | #limits-descriptorBufferAddressSpaceSize#
    -- @descriptorBufferAddressSpaceSize@ indicates the total size in bytes of
    -- the address space available for descriptor buffers created with both
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT'
    -- and
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'.
    descriptorBufferAddressSpaceSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorBufferPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceDescriptorBufferPropertiesEXT

instance ToCStruct PhysicalDeviceDescriptorBufferPropertiesEXT where
  withCStruct x f = allocaBytes 256 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorBufferPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (combinedImageSamplerDescriptorSingleArray))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (bufferlessPushDescriptors))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (allowSamplerImageViewPostSubmitCreation))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (descriptorBufferOffsetAlignment)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxDescriptorBufferBindings)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxResourceDescriptorBufferBindings)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxSamplerDescriptorBufferBindings)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (maxEmbeddedImmutableSamplerBindings)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxEmbeddedImmutableSamplers)
    poke ((p `plusPtr` 64 :: Ptr CSize)) (CSize (bufferCaptureReplayDescriptorDataSize))
    poke ((p `plusPtr` 72 :: Ptr CSize)) (CSize (imageCaptureReplayDescriptorDataSize))
    poke ((p `plusPtr` 80 :: Ptr CSize)) (CSize (imageViewCaptureReplayDescriptorDataSize))
    poke ((p `plusPtr` 88 :: Ptr CSize)) (CSize (samplerCaptureReplayDescriptorDataSize))
    poke ((p `plusPtr` 96 :: Ptr CSize)) (CSize (accelerationStructureCaptureReplayDescriptorDataSize))
    poke ((p `plusPtr` 104 :: Ptr CSize)) (CSize (samplerDescriptorSize))
    poke ((p `plusPtr` 112 :: Ptr CSize)) (CSize (combinedImageSamplerDescriptorSize))
    poke ((p `plusPtr` 120 :: Ptr CSize)) (CSize (sampledImageDescriptorSize))
    poke ((p `plusPtr` 128 :: Ptr CSize)) (CSize (storageImageDescriptorSize))
    poke ((p `plusPtr` 136 :: Ptr CSize)) (CSize (uniformTexelBufferDescriptorSize))
    poke ((p `plusPtr` 144 :: Ptr CSize)) (CSize (robustUniformTexelBufferDescriptorSize))
    poke ((p `plusPtr` 152 :: Ptr CSize)) (CSize (storageTexelBufferDescriptorSize))
    poke ((p `plusPtr` 160 :: Ptr CSize)) (CSize (robustStorageTexelBufferDescriptorSize))
    poke ((p `plusPtr` 168 :: Ptr CSize)) (CSize (uniformBufferDescriptorSize))
    poke ((p `plusPtr` 176 :: Ptr CSize)) (CSize (robustUniformBufferDescriptorSize))
    poke ((p `plusPtr` 184 :: Ptr CSize)) (CSize (storageBufferDescriptorSize))
    poke ((p `plusPtr` 192 :: Ptr CSize)) (CSize (robustStorageBufferDescriptorSize))
    poke ((p `plusPtr` 200 :: Ptr CSize)) (CSize (inputAttachmentDescriptorSize))
    poke ((p `plusPtr` 208 :: Ptr CSize)) (CSize (accelerationStructureDescriptorSize))
    poke ((p `plusPtr` 216 :: Ptr DeviceSize)) (maxSamplerDescriptorBufferRange)
    poke ((p `plusPtr` 224 :: Ptr DeviceSize)) (maxResourceDescriptorBufferRange)
    poke ((p `plusPtr` 232 :: Ptr DeviceSize)) (samplerDescriptorBufferAddressSpaceSize)
    poke ((p `plusPtr` 240 :: Ptr DeviceSize)) (resourceDescriptorBufferAddressSpaceSize)
    poke ((p `plusPtr` 248 :: Ptr DeviceSize)) (descriptorBufferAddressSpaceSize)
    f
  cStructSize = 256
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 72 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 80 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 88 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 96 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 104 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 112 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 120 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 128 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 136 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 144 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 152 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 160 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 168 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 176 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 184 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 192 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 200 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 208 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 216 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 224 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 232 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 240 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 248 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceDescriptorBufferPropertiesEXT where
  peekCStruct p = do
    combinedImageSamplerDescriptorSingleArray <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    bufferlessPushDescriptors <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    allowSamplerImageViewPostSubmitCreation <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    descriptorBufferOffsetAlignment <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    maxDescriptorBufferBindings <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxResourceDescriptorBufferBindings <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    maxSamplerDescriptorBufferBindings <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxEmbeddedImmutableSamplerBindings <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    maxEmbeddedImmutableSamplers <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    bufferCaptureReplayDescriptorDataSize <- peek @CSize ((p `plusPtr` 64 :: Ptr CSize))
    imageCaptureReplayDescriptorDataSize <- peek @CSize ((p `plusPtr` 72 :: Ptr CSize))
    imageViewCaptureReplayDescriptorDataSize <- peek @CSize ((p `plusPtr` 80 :: Ptr CSize))
    samplerCaptureReplayDescriptorDataSize <- peek @CSize ((p `plusPtr` 88 :: Ptr CSize))
    accelerationStructureCaptureReplayDescriptorDataSize <- peek @CSize ((p `plusPtr` 96 :: Ptr CSize))
    samplerDescriptorSize <- peek @CSize ((p `plusPtr` 104 :: Ptr CSize))
    combinedImageSamplerDescriptorSize <- peek @CSize ((p `plusPtr` 112 :: Ptr CSize))
    sampledImageDescriptorSize <- peek @CSize ((p `plusPtr` 120 :: Ptr CSize))
    storageImageDescriptorSize <- peek @CSize ((p `plusPtr` 128 :: Ptr CSize))
    uniformTexelBufferDescriptorSize <- peek @CSize ((p `plusPtr` 136 :: Ptr CSize))
    robustUniformTexelBufferDescriptorSize <- peek @CSize ((p `plusPtr` 144 :: Ptr CSize))
    storageTexelBufferDescriptorSize <- peek @CSize ((p `plusPtr` 152 :: Ptr CSize))
    robustStorageTexelBufferDescriptorSize <- peek @CSize ((p `plusPtr` 160 :: Ptr CSize))
    uniformBufferDescriptorSize <- peek @CSize ((p `plusPtr` 168 :: Ptr CSize))
    robustUniformBufferDescriptorSize <- peek @CSize ((p `plusPtr` 176 :: Ptr CSize))
    storageBufferDescriptorSize <- peek @CSize ((p `plusPtr` 184 :: Ptr CSize))
    robustStorageBufferDescriptorSize <- peek @CSize ((p `plusPtr` 192 :: Ptr CSize))
    inputAttachmentDescriptorSize <- peek @CSize ((p `plusPtr` 200 :: Ptr CSize))
    accelerationStructureDescriptorSize <- peek @CSize ((p `plusPtr` 208 :: Ptr CSize))
    maxSamplerDescriptorBufferRange <- peek @DeviceSize ((p `plusPtr` 216 :: Ptr DeviceSize))
    maxResourceDescriptorBufferRange <- peek @DeviceSize ((p `plusPtr` 224 :: Ptr DeviceSize))
    samplerDescriptorBufferAddressSpaceSize <- peek @DeviceSize ((p `plusPtr` 232 :: Ptr DeviceSize))
    resourceDescriptorBufferAddressSpaceSize <- peek @DeviceSize ((p `plusPtr` 240 :: Ptr DeviceSize))
    descriptorBufferAddressSpaceSize <- peek @DeviceSize ((p `plusPtr` 248 :: Ptr DeviceSize))
    pure $ PhysicalDeviceDescriptorBufferPropertiesEXT
             (bool32ToBool combinedImageSamplerDescriptorSingleArray)
             (bool32ToBool bufferlessPushDescriptors)
             (bool32ToBool allowSamplerImageViewPostSubmitCreation)
             descriptorBufferOffsetAlignment
             maxDescriptorBufferBindings
             maxResourceDescriptorBufferBindings
             maxSamplerDescriptorBufferBindings
             maxEmbeddedImmutableSamplerBindings
             maxEmbeddedImmutableSamplers
             (coerce @CSize @Word64 bufferCaptureReplayDescriptorDataSize)
             (coerce @CSize @Word64 imageCaptureReplayDescriptorDataSize)
             (coerce @CSize @Word64 imageViewCaptureReplayDescriptorDataSize)
             (coerce @CSize @Word64 samplerCaptureReplayDescriptorDataSize)
             (coerce @CSize @Word64 accelerationStructureCaptureReplayDescriptorDataSize)
             (coerce @CSize @Word64 samplerDescriptorSize)
             (coerce @CSize @Word64 combinedImageSamplerDescriptorSize)
             (coerce @CSize @Word64 sampledImageDescriptorSize)
             (coerce @CSize @Word64 storageImageDescriptorSize)
             (coerce @CSize @Word64 uniformTexelBufferDescriptorSize)
             (coerce @CSize @Word64 robustUniformTexelBufferDescriptorSize)
             (coerce @CSize @Word64 storageTexelBufferDescriptorSize)
             (coerce @CSize @Word64 robustStorageTexelBufferDescriptorSize)
             (coerce @CSize @Word64 uniformBufferDescriptorSize)
             (coerce @CSize @Word64 robustUniformBufferDescriptorSize)
             (coerce @CSize @Word64 storageBufferDescriptorSize)
             (coerce @CSize @Word64 robustStorageBufferDescriptorSize)
             (coerce @CSize @Word64 inputAttachmentDescriptorSize)
             (coerce @CSize @Word64 accelerationStructureDescriptorSize)
             maxSamplerDescriptorBufferRange
             maxResourceDescriptorBufferRange
             samplerDescriptorBufferAddressSpaceSize
             resourceDescriptorBufferAddressSpaceSize
             descriptorBufferAddressSpaceSize

instance Storable PhysicalDeviceDescriptorBufferPropertiesEXT where
  sizeOf ~_ = 256
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorBufferPropertiesEXT where
  zero = PhysicalDeviceDescriptorBufferPropertiesEXT
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
           zero
           zero
           zero


-- | VkPhysicalDeviceDescriptorBufferDensityMapPropertiesEXT - Structure
-- describing descriptor buffer density map properties supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT = PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT
  { -- | #limits-combinedImageSamplerDensityMapDescriptorSize#
    -- @combinedImageSamplerDensityMapDescriptorSize@ indicates the size in
    -- bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
    -- descriptor when creating the descriptor with
    -- 'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT'
    -- set.
    combinedImageSamplerDensityMapDescriptorSize :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT

instance ToCStruct PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_DENSITY_MAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (combinedImageSamplerDensityMapDescriptorSize))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_DENSITY_MAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT where
  peekCStruct p = do
    combinedImageSamplerDensityMapDescriptorSize <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    pure $ PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT
             (coerce @CSize @Word64 combinedImageSamplerDensityMapDescriptorSize)

instance Storable PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT where
  zero = PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT
           zero


-- | VkDescriptorAddressInfoEXT - Structure specifying descriptor buffer
-- address info
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorAddressInfoEXT-address-08043# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, @address@ /must/ not be zero
--
-- -   #VUID-VkDescriptorAddressInfoEXT-nullDescriptor-08938# If @address@
--     is zero, @range@ /must/ be 'Vulkan.Core10.APIConstants.WHOLE_SIZE'
--
-- -   #VUID-VkDescriptorAddressInfoEXT-nullDescriptor-08939# If @address@
--     is not zero, @range@ /must/ not be
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE'
--
-- -   #VUID-VkDescriptorAddressInfoEXT-None-08044# If @address@ is not
--     zero, @address@ /must/ be a valid device address at an offset within
--     a 'Vulkan.Core10.Handles.Buffer'
--
-- -   #VUID-VkDescriptorAddressInfoEXT-range-08045# @range@ /must/ be less
--     than or equal to the size of the buffer containing @address@ minus
--     the offset of @address@ from the base address of the buffer
--
-- -   #VUID-VkDescriptorAddressInfoEXT-range-08940# @range@ /must/ not be
--     zero
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorAddressInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT'
--
-- -   #VUID-VkDescriptorAddressInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDescriptorAddressInfoEXT-format-parameter# @format@ /must/
--     be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
-- feature is enabled, @address@ /can/ be zero. Loads from a null
-- descriptor return zero values and stores and atomics to a null
-- descriptor are discarded.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'DescriptorDataEXT', 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DescriptorAddressInfoEXT = DescriptorAddressInfoEXT
  { -- | @address@ is either @0@ or a device address at an offset in a buffer,
    -- where the base address can be queried from
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'.
    address :: DeviceAddress
  , -- | @range@ is the size in bytes of the buffer or buffer view used by the
    -- descriptor.
    range :: DeviceSize
  , -- | @format@ is the format of the data elements in the buffer view and is
    -- ignored for buffers.
    format :: Format
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorAddressInfoEXT)
#endif
deriving instance Show DescriptorAddressInfoEXT

instance ToCStruct DescriptorAddressInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorAddressInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (address)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (range)
    poke ((p `plusPtr` 32 :: Ptr Format)) (format)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    f

instance FromCStruct DescriptorAddressInfoEXT where
  peekCStruct p = do
    address <- peek @DeviceAddress ((p `plusPtr` 16 :: Ptr DeviceAddress))
    range <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    format <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    pure $ DescriptorAddressInfoEXT
             address range format

instance Storable DescriptorAddressInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorAddressInfoEXT where
  zero = DescriptorAddressInfoEXT
           zero
           zero
           zero


-- | VkDescriptorBufferBindingInfoEXT - Structure specifying descriptor
-- buffer binding information
--
-- = Description
--
-- If a
-- 'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'
-- structure is present in the @pNext@ chain,
-- 'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
-- from that structure is used instead of @flags@ from this structure.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-bufferlessPushDescriptors-08056#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-bufferlessPushDescriptors ::bufferlessPushDescriptors>
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and @usage@ contains
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT',
--     then the @pNext@ chain /must/ include a
--     'DescriptorBufferBindingPushDescriptorBufferHandleEXT' structure
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-address-08057# @address@
--     /must/ be aligned to
--     'PhysicalDeviceDescriptorBufferPropertiesEXT'::@descriptorBufferOffsetAlignment@
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-usage-08122# If @usage@
--     includes
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT',
--     @address@ /must/ be an address within a valid buffer that was
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-usage-08123# If @usage@
--     includes
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT',
--     @address@ /must/ be an address within a valid buffer that was
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-usage-08124# If @usage@
--     includes
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT',
--     @address@ /must/ be an address within a valid buffer that was
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_INFO_EXT'
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_maintenance5.BufferUsageFlags2CreateInfoKHR'
--     or 'DescriptorBufferBindingPushDescriptorBufferHandleEXT'
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-sType-unique# The @sType@
--     value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-usage-parameter# @usage@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits' values
--
-- -   #VUID-VkDescriptorBufferBindingInfoEXT-usage-requiredbitmask#
--     @usage@ /must/ not be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlags',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBindDescriptorBuffersEXT'
data DescriptorBufferBindingInfoEXT (es :: [Type]) = DescriptorBufferBindingInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @address@ is a 'Vulkan.Core10.FundamentalTypes.DeviceAddress' specifying
    -- the device address defining the descriptor buffer to be bound.
    address :: DeviceAddress
  , -- | @usage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits' specifying
    -- the 'Vulkan.Core10.Buffer.BufferCreateInfo'::@usage@ for the buffer from
    -- which @address@ was queried.
    usage :: BufferUsageFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorBufferBindingInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorBufferBindingInfoEXT es)

instance Extensible DescriptorBufferBindingInfoEXT where
  extensibleTypeName = "DescriptorBufferBindingInfoEXT"
  setNext DescriptorBufferBindingInfoEXT{..} next' = DescriptorBufferBindingInfoEXT{next = next', ..}
  getNext DescriptorBufferBindingInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorBufferBindingInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DescriptorBufferBindingPushDescriptorBufferHandleEXT = Just f
    | Just Refl <- eqT @e @BufferUsageFlags2CreateInfoKHR = Just f
    | otherwise = Nothing

instance ( Extendss DescriptorBufferBindingInfoEXT es
         , PokeChain es ) => ToCStruct (DescriptorBufferBindingInfoEXT es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorBufferBindingInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (address)
    lift $ poke ((p `plusPtr` 24 :: Ptr BufferUsageFlags)) (usage)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr BufferUsageFlags)) (zero)
    lift $ f

instance ( Extendss DescriptorBufferBindingInfoEXT es
         , PeekChain es ) => FromCStruct (DescriptorBufferBindingInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    address <- peek @DeviceAddress ((p `plusPtr` 16 :: Ptr DeviceAddress))
    usage <- peek @BufferUsageFlags ((p `plusPtr` 24 :: Ptr BufferUsageFlags))
    pure $ DescriptorBufferBindingInfoEXT
             next address usage

instance es ~ '[] => Zero (DescriptorBufferBindingInfoEXT es) where
  zero = DescriptorBufferBindingInfoEXT
           ()
           zero
           zero


-- | VkDescriptorBufferBindingPushDescriptorBufferHandleEXT - Structure
-- specifying push descriptor buffer binding information
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorBufferBindingPushDescriptorBufferHandleEXT-bufferlessPushDescriptors-08059#
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-bufferlessPushDescriptors ::bufferlessPushDescriptors>
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorBufferBindingPushDescriptorBufferHandleEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT'
--
-- -   #VUID-VkDescriptorBufferBindingPushDescriptorBufferHandleEXT-buffer-parameter#
--     @buffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DescriptorBufferBindingPushDescriptorBufferHandleEXT = DescriptorBufferBindingPushDescriptorBufferHandleEXT
  { -- | @buffer@ is the 'Vulkan.Core10.Handles.Buffer' handle of the buffer for
    -- push descriptors.
    buffer :: Buffer }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorBufferBindingPushDescriptorBufferHandleEXT)
#endif
deriving instance Show DescriptorBufferBindingPushDescriptorBufferHandleEXT

instance ToCStruct DescriptorBufferBindingPushDescriptorBufferHandleEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorBufferBindingPushDescriptorBufferHandleEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (buffer)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    f

instance FromCStruct DescriptorBufferBindingPushDescriptorBufferHandleEXT where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    pure $ DescriptorBufferBindingPushDescriptorBufferHandleEXT
             buffer

instance Storable DescriptorBufferBindingPushDescriptorBufferHandleEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorBufferBindingPushDescriptorBufferHandleEXT where
  zero = DescriptorBufferBindingPushDescriptorBufferHandleEXT
           zero


-- | VkDescriptorGetInfoEXT - Structure specifying parameters of descriptor
-- to get
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08018# @type@ /must/ not be
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08019# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the @pCombinedImageSampler->sampler@ member of @data@ /must/ be a
--     'Vulkan.Core10.Handles.Sampler' created on @device@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08020# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the @pCombinedImageSampler->imageView@ member of @data@ /must/ be a
--     'Vulkan.Core10.Handles.ImageView' created on @device@, or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08021# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     the @pInputAttachmentImage->imageView@ member of @data@ /must/ be a
--     'Vulkan.Core10.Handles.ImageView' created on @device@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08022# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     and if @pSampledImage@ is not @NULL@, the @pSampledImage->imageView@
--     member of @data@ /must/ be a 'Vulkan.Core10.Handles.ImageView'
--     created on @device@, or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08023# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     and if @pStorageImage@ is not @NULL@, the @pStorageImage->imageView@
--     member of @data@ /must/ be a 'Vulkan.Core10.Handles.ImageView'
--     created on @device@, or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08024# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
--     @pUniformTexelBuffer@ is not @NULL@ and
--     @pUniformTexelBuffer->address@ is not zero,
--     @pUniformTexelBuffer->address@ must be an address within a
--     'Vulkan.Core10.Handles.Buffer' created on @device@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08025# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     @pStorageTexelBuffer@ is not @NULL@ and
--     @pStorageTexelBuffer->address@ is not zero,
--     @pStorageTexelBuffer->address@ must be an address within a
--     'Vulkan.Core10.Handles.Buffer' created on @device@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08026# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     @pUniformBuffer@ is not @NULL@ and @pUniformBuffer->address@ is not
--     zero, @pUniformBuffer->address@ must be an address within a
--     'Vulkan.Core10.Handles.Buffer' created on @device@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08027# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     @pStorageBuffer@ is not @NULL@ and @pStorageBuffer->address@ is not
--     zero, @pStorageBuffer->address@ must be an address within a
--     'Vulkan.Core10.Handles.Buffer' created on @device@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-09427# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
--     @pUniformBuffer@ is not @NULL@ , the number of texel buffer elements
--     given by (⌊@pUniformBuffer->range@ \/ (texel block size)⌋ × (texels
--     per block)) where texel block size and texels per block are as
--     defined in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility Compatible Formats>
--     table for @pUniformBuffer->format@, /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTexelBufferElements@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-09428# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     @pStorageBuffer@ is not @NULL@ , the number of texel buffer elements
--     given by (⌊@pStorageBuffer->range@ \/ (texel block size)⌋ × (texels
--     per block)) where texel block size and texels per block are as
--     defined in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility Compatible Formats>
--     table for @pStorageBuffer->format@, /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTexelBufferElements@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08028# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR'
--     and @accelerationStructure@ is not @0@, @accelerationStructure@
--     /must/ contain the address of a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' created on
--     @device@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-08029# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV'
--     and @accelerationStructure@ is not @0@, @accelerationStructure@
--     /must/ contain the handle of a
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' created on
--     @device@, returned by
--     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorGetInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT'
--
-- -   #VUID-VkDescriptorGetInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkDescriptorGetInfoEXT-type-parameter# @type@ /must/ be a
--     valid 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
--
-- -   #VUID-VkDescriptorGetInfoEXT-pSampler-parameter# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER', the
--     @pSampler@ member of @data@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.Handles.Sampler' handle
--
-- -   #VUID-VkDescriptorGetInfoEXT-pCombinedImageSampler-parameter# If
--     @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the @pCombinedImageSampler@ member of @data@ /must/ be a valid
--     pointer to a valid 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo'
--     structure
--
-- -   #VUID-VkDescriptorGetInfoEXT-pInputAttachmentImage-parameter# If
--     @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     the @pInputAttachmentImage@ member of @data@ /must/ be a valid
--     pointer to a valid 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo'
--     structure
--
-- -   #VUID-VkDescriptorGetInfoEXT-pSampledImage-parameter# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     and if @pSampledImage@ is not @NULL@, the @pSampledImage@ member of
--     @data@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.DescriptorSet.DescriptorImageInfo' structure
--
-- -   #VUID-VkDescriptorGetInfoEXT-pStorageImage-parameter# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     and if @pStorageImage@ is not @NULL@, the @pStorageImage@ member of
--     @data@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.DescriptorSet.DescriptorImageInfo' structure
--
-- -   #VUID-VkDescriptorGetInfoEXT-pUniformTexelBuffer-parameter# If
--     @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
--     and if @pUniformTexelBuffer@ is not @NULL@, the
--     @pUniformTexelBuffer@ member of @data@ /must/ be a valid pointer to
--     a valid 'DescriptorAddressInfoEXT' structure
--
-- -   #VUID-VkDescriptorGetInfoEXT-pStorageTexelBuffer-parameter# If
--     @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     and if @pStorageTexelBuffer@ is not @NULL@, the
--     @pStorageTexelBuffer@ member of @data@ /must/ be a valid pointer to
--     a valid 'DescriptorAddressInfoEXT' structure
--
-- -   #VUID-VkDescriptorGetInfoEXT-pUniformBuffer-parameter# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     and if @pUniformBuffer@ is not @NULL@, the @pUniformBuffer@ member
--     of @data@ /must/ be a valid pointer to a valid
--     'DescriptorAddressInfoEXT' structure
--
-- -   #VUID-VkDescriptorGetInfoEXT-pStorageBuffer-parameter# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     and if @pStorageBuffer@ is not @NULL@, the @pStorageBuffer@ member
--     of @data@ /must/ be a valid pointer to a valid
--     'DescriptorAddressInfoEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'DescriptorDataEXT',
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'getDescriptorEXT'
data DescriptorGetInfoEXT = DescriptorGetInfoEXT
  { -- | @type@ is the type of descriptor to get.
    type' :: DescriptorType
  , -- | @data@ is a structure containing the information needed to get the
    -- descriptor.
    data' :: DescriptorDataEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorGetInfoEXT)
#endif
deriving instance Show DescriptorGetInfoEXT

instance ToCStruct DescriptorGetInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorGetInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorType)) (type')
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DescriptorDataEXT)) (data') . ($ ())
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorType)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DescriptorDataEXT)) (zero) . ($ ())
    lift $ f

instance FromCStruct DescriptorGetInfoEXT where
  peekCStruct p = do
    type' <- peek @DescriptorType ((p `plusPtr` 16 :: Ptr DescriptorType))
    data' <- peekDescriptorDataEXT type' ((p `plusPtr` 24 :: Ptr DescriptorDataEXT))
    pure $ DescriptorGetInfoEXT
             type' data'

instance Zero DescriptorGetInfoEXT where
  zero = DescriptorGetInfoEXT
           zero
           zero


-- | VkBufferCaptureDescriptorDataInfoEXT - Structure specifying a buffer for
-- descriptor capture
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getBufferOpaqueCaptureDescriptorDataEXT'
data BufferCaptureDescriptorDataInfoEXT = BufferCaptureDescriptorDataInfoEXT
  { -- | @buffer@ is the 'Vulkan.Core10.Handles.Buffer' handle of the buffer to
    -- get opaque capture data for.
    --
    -- #VUID-VkBufferCaptureDescriptorDataInfoEXT-buffer-08075# @buffer@ /must/
    -- have been created with
    -- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
    -- set in 'Vulkan.Core10.Buffer.BufferCreateInfo'::@flags@
    --
    -- #VUID-VkBufferCaptureDescriptorDataInfoEXT-buffer-parameter# @buffer@
    -- /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
    buffer :: Buffer }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCaptureDescriptorDataInfoEXT)
#endif
deriving instance Show BufferCaptureDescriptorDataInfoEXT

instance ToCStruct BufferCaptureDescriptorDataInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCaptureDescriptorDataInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (buffer)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    f

instance FromCStruct BufferCaptureDescriptorDataInfoEXT where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    pure $ BufferCaptureDescriptorDataInfoEXT
             buffer

instance Storable BufferCaptureDescriptorDataInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCaptureDescriptorDataInfoEXT where
  zero = BufferCaptureDescriptorDataInfoEXT
           zero


-- | VkImageCaptureDescriptorDataInfoEXT - Structure specifying an image for
-- descriptor capture
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageOpaqueCaptureDescriptorDataEXT'
data ImageCaptureDescriptorDataInfoEXT = ImageCaptureDescriptorDataInfoEXT
  { -- | @image@ is the 'Vulkan.Core10.Handles.Image' handle of the image to get
    -- opaque capture data for.
    --
    -- #VUID-VkImageCaptureDescriptorDataInfoEXT-image-08079# @image@ /must/
    -- have been created with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
    -- set in 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@
    --
    -- #VUID-VkImageCaptureDescriptorDataInfoEXT-image-parameter# @image@
    -- /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
    image :: Image }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCaptureDescriptorDataInfoEXT)
#endif
deriving instance Show ImageCaptureDescriptorDataInfoEXT

instance ToCStruct ImageCaptureDescriptorDataInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCaptureDescriptorDataInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    f

instance FromCStruct ImageCaptureDescriptorDataInfoEXT where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    pure $ ImageCaptureDescriptorDataInfoEXT
             image

instance Storable ImageCaptureDescriptorDataInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCaptureDescriptorDataInfoEXT where
  zero = ImageCaptureDescriptorDataInfoEXT
           zero


-- | VkImageViewCaptureDescriptorDataInfoEXT - Structure specifying an image
-- view for descriptor capture
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.ImageView',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageViewOpaqueCaptureDescriptorDataEXT'
data ImageViewCaptureDescriptorDataInfoEXT = ImageViewCaptureDescriptorDataInfoEXT
  { -- | @imageView@ is the 'Vulkan.Core10.Handles.ImageView' handle of the image
    -- view to get opaque capture data for.
    --
    -- #VUID-VkImageViewCaptureDescriptorDataInfoEXT-imageView-08083#
    -- @imageView@ /must/ have been created with
    -- 'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
    -- set in 'Vulkan.Core10.ImageView.ImageViewCreateInfo'::@flags@
    --
    -- #VUID-VkImageViewCaptureDescriptorDataInfoEXT-imageView-parameter#
    -- @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView' handle
    imageView :: ImageView }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewCaptureDescriptorDataInfoEXT)
#endif
deriving instance Show ImageViewCaptureDescriptorDataInfoEXT

instance ToCStruct ImageViewCaptureDescriptorDataInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewCaptureDescriptorDataInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (imageView)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (zero)
    f

instance FromCStruct ImageViewCaptureDescriptorDataInfoEXT where
  peekCStruct p = do
    imageView <- peek @ImageView ((p `plusPtr` 16 :: Ptr ImageView))
    pure $ ImageViewCaptureDescriptorDataInfoEXT
             imageView

instance Storable ImageViewCaptureDescriptorDataInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewCaptureDescriptorDataInfoEXT where
  zero = ImageViewCaptureDescriptorDataInfoEXT
           zero


-- | VkSamplerCaptureDescriptorDataInfoEXT - Structure specifying a sampler
-- for descriptor capture
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Sampler',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getSamplerOpaqueCaptureDescriptorDataEXT'
data SamplerCaptureDescriptorDataInfoEXT = SamplerCaptureDescriptorDataInfoEXT
  { -- | @sampler@ is the 'Vulkan.Core10.Handles.Sampler' handle of the sampler
    -- to get opaque capture data for.
    --
    -- #VUID-VkSamplerCaptureDescriptorDataInfoEXT-sampler-08087# @sampler@
    -- /must/ have been created with
    -- 'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
    -- set in 'Vulkan.Core10.Sampler.SamplerCreateInfo'::@flags@
    --
    -- #VUID-VkSamplerCaptureDescriptorDataInfoEXT-sampler-parameter# @sampler@
    -- /must/ be a valid 'Vulkan.Core10.Handles.Sampler' handle
    sampler :: Sampler }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerCaptureDescriptorDataInfoEXT)
#endif
deriving instance Show SamplerCaptureDescriptorDataInfoEXT

instance ToCStruct SamplerCaptureDescriptorDataInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerCaptureDescriptorDataInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Sampler)) (sampler)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Sampler)) (zero)
    f

instance FromCStruct SamplerCaptureDescriptorDataInfoEXT where
  peekCStruct p = do
    sampler <- peek @Sampler ((p `plusPtr` 16 :: Ptr Sampler))
    pure $ SamplerCaptureDescriptorDataInfoEXT
             sampler

instance Storable SamplerCaptureDescriptorDataInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerCaptureDescriptorDataInfoEXT where
  zero = SamplerCaptureDescriptorDataInfoEXT
           zero


-- | VkAccelerationStructureCaptureDescriptorDataInfoEXT - Structure
-- specifying an acceleration structure for descriptor capture
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-accelerationStructure-08091#
--     If @accelerationStructure@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' then
--     @accelerationStructure@ /must/ have been created with
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--     set in
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR'::@createFlags@
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-accelerationStructureNV-08092#
--     If @accelerationStructureNV@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' then
--     @accelerationStructureNV@ /must/ have been created with
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--     set in
--     'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV'::@info.flags@
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-accelerationStructure-08093#
--     If @accelerationStructure@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' then
--     @accelerationStructureNV@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-accelerationStructureNV-08094#
--     If @accelerationStructureNV@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' then
--     @accelerationStructure@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-pNext-pNext#
--     @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-accelerationStructure-parameter#
--     If @accelerationStructure@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @accelerationStructure@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-accelerationStructureNV-parameter#
--     If @accelerationStructureNV@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @accelerationStructureNV@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   #VUID-VkAccelerationStructureCaptureDescriptorDataInfoEXT-commonparent#
--     Both of @accelerationStructure@, and @accelerationStructureNV@ that
--     are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>,
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getAccelerationStructureOpaqueCaptureDescriptorDataEXT'
data AccelerationStructureCaptureDescriptorDataInfoEXT = AccelerationStructureCaptureDescriptorDataInfoEXT
  { -- | @accelerationStructure@ is the
    -- 'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle of the
    -- acceleration structure to get opaque capture data for.
    accelerationStructure :: AccelerationStructureKHR
  , -- | @accelerationStructureNV@ is the
    -- 'Vulkan.Extensions.Handles.AccelerationStructureNV' handle of the
    -- acceleration structure to get opaque capture data for.
    accelerationStructureNV :: AccelerationStructureNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureCaptureDescriptorDataInfoEXT)
#endif
deriving instance Show AccelerationStructureCaptureDescriptorDataInfoEXT

instance ToCStruct AccelerationStructureCaptureDescriptorDataInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureCaptureDescriptorDataInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (accelerationStructure)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureNV)) (accelerationStructureNV)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct AccelerationStructureCaptureDescriptorDataInfoEXT where
  peekCStruct p = do
    accelerationStructure <- peek @AccelerationStructureKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR))
    accelerationStructureNV <- peek @AccelerationStructureNV ((p `plusPtr` 24 :: Ptr AccelerationStructureNV))
    pure $ AccelerationStructureCaptureDescriptorDataInfoEXT
             accelerationStructure accelerationStructureNV

instance Storable AccelerationStructureCaptureDescriptorDataInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureCaptureDescriptorDataInfoEXT where
  zero = AccelerationStructureCaptureDescriptorDataInfoEXT
           zero
           zero


-- | VkOpaqueCaptureDescriptorDataCreateInfoEXT - Structure specifying opaque
-- capture descriptor data
--
-- = Description
--
-- During replay, opaque descriptor capture data /can/ be specified by
-- adding a 'OpaqueCaptureDescriptorDataCreateInfoEXT' structure to the
-- relevant @pNext@ chain of a 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core10.ImageView.ImageViewCreateInfo',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV'
-- or
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR'
-- structure.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data OpaqueCaptureDescriptorDataCreateInfoEXT = OpaqueCaptureDescriptorDataCreateInfoEXT
  { -- | @opaqueCaptureDescriptorData@ is a pointer to a user-allocated buffer
    -- containing opaque capture data retrieved using
    -- 'getBufferOpaqueCaptureDescriptorDataEXT',
    -- 'getImageOpaqueCaptureDescriptorDataEXT',
    -- 'getImageViewOpaqueCaptureDescriptorDataEXT',
    -- 'getSamplerOpaqueCaptureDescriptorDataEXT', or
    -- 'getAccelerationStructureOpaqueCaptureDescriptorDataEXT'.
    --
    -- #VUID-VkOpaqueCaptureDescriptorDataCreateInfoEXT-opaqueCaptureDescriptorData-parameter#
    -- @opaqueCaptureDescriptorData@ /must/ be a pointer value
    opaqueCaptureDescriptorData :: Ptr () }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (OpaqueCaptureDescriptorDataCreateInfoEXT)
#endif
deriving instance Show OpaqueCaptureDescriptorDataCreateInfoEXT

instance ToCStruct OpaqueCaptureDescriptorDataCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p OpaqueCaptureDescriptorDataCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (opaqueCaptureDescriptorData)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct OpaqueCaptureDescriptorDataCreateInfoEXT where
  peekCStruct p = do
    opaqueCaptureDescriptorData <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    pure $ OpaqueCaptureDescriptorDataCreateInfoEXT
             opaqueCaptureDescriptorData

instance Storable OpaqueCaptureDescriptorDataCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero OpaqueCaptureDescriptorDataCreateInfoEXT where
  zero = OpaqueCaptureDescriptorDataCreateInfoEXT
           zero


data DescriptorDataEXT
  = ASampler Sampler
  | ACombinedImageSampler DescriptorImageInfo
  | AnInputAttachmentImage DescriptorImageInfo
  | ASampledImage (Maybe DescriptorImageInfo)
  | AStorageImage (Maybe DescriptorImageInfo)
  | AnUniformTexelBuffer (Maybe DescriptorAddressInfoEXT)
  | AStorageTexelBuffer (Maybe DescriptorAddressInfoEXT)
  | AnUniformBuffer (Maybe DescriptorAddressInfoEXT)
  | AStorageBuffer (Maybe DescriptorAddressInfoEXT)
  | AnAccelerationStructure DeviceAddress
  deriving (Show)

instance ToCStruct DescriptorDataEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DescriptorDataEXT -> DescriptorDataEXT -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    ASampler v -> do
      pSampler <- ContT $ with (v)
      lift $ poke (castPtr @_ @(Ptr Sampler) p) pSampler
    ACombinedImageSampler v -> do
      pCombinedImageSampler <- ContT $ withCStruct (v)
      lift $ poke (castPtr @_ @(Ptr DescriptorImageInfo) p) pCombinedImageSampler
    AnInputAttachmentImage v -> do
      pInputAttachmentImage <- ContT $ withCStruct (v)
      lift $ poke (castPtr @_ @(Ptr DescriptorImageInfo) p) pInputAttachmentImage
    ASampledImage v -> do
      pSampledImage <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr DescriptorImageInfo) p) pSampledImage
    AStorageImage v -> do
      pStorageImage <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr DescriptorImageInfo) p) pStorageImage
    AnUniformTexelBuffer v -> do
      pUniformTexelBuffer <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr DescriptorAddressInfoEXT) p) pUniformTexelBuffer
    AStorageTexelBuffer v -> do
      pStorageTexelBuffer <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr DescriptorAddressInfoEXT) p) pStorageTexelBuffer
    AnUniformBuffer v -> do
      pUniformBuffer <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr DescriptorAddressInfoEXT) p) pUniformBuffer
    AStorageBuffer v -> do
      pStorageBuffer <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr DescriptorAddressInfoEXT) p) pStorageBuffer
    AnAccelerationStructure v -> lift $ poke (castPtr @_ @DeviceAddress p) (v)
  pokeZeroCStruct :: Ptr DescriptorDataEXT -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero DescriptorDataEXT where
  zero = ASampler zero

peekDescriptorDataEXT :: DescriptorType -> Ptr DescriptorDataEXT -> IO DescriptorDataEXT
peekDescriptorDataEXT tag p = case tag of
  DESCRIPTOR_TYPE_SAMPLER -> ASampler <$> (peek @Sampler =<< peek (castPtr @_ @(Ptr Sampler) p))
  DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER -> ACombinedImageSampler <$> (peekCStruct @DescriptorImageInfo =<< peek (castPtr @_ @(Ptr DescriptorImageInfo) p))
  DESCRIPTOR_TYPE_INPUT_ATTACHMENT -> AnInputAttachmentImage <$> (peekCStruct @DescriptorImageInfo =<< peek (castPtr @_ @(Ptr DescriptorImageInfo) p))
  DESCRIPTOR_TYPE_SAMPLED_IMAGE -> ASampledImage <$> (do
    pSampledImage <- peek @(Ptr DescriptorImageInfo) (castPtr @_ @(Ptr DescriptorImageInfo) p)
    maybePeek (\j -> peekCStruct @DescriptorImageInfo (j)) pSampledImage)
  DESCRIPTOR_TYPE_STORAGE_IMAGE -> AStorageImage <$> (do
    pStorageImage <- peek @(Ptr DescriptorImageInfo) (castPtr @_ @(Ptr DescriptorImageInfo) p)
    maybePeek (\j -> peekCStruct @DescriptorImageInfo (j)) pStorageImage)
  DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER -> AnUniformTexelBuffer <$> (do
    pUniformTexelBuffer <- peek @(Ptr DescriptorAddressInfoEXT) (castPtr @_ @(Ptr DescriptorAddressInfoEXT) p)
    maybePeek (\j -> peekCStruct @DescriptorAddressInfoEXT (j)) pUniformTexelBuffer)
  DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER -> AStorageTexelBuffer <$> (do
    pStorageTexelBuffer <- peek @(Ptr DescriptorAddressInfoEXT) (castPtr @_ @(Ptr DescriptorAddressInfoEXT) p)
    maybePeek (\j -> peekCStruct @DescriptorAddressInfoEXT (j)) pStorageTexelBuffer)
  DESCRIPTOR_TYPE_UNIFORM_BUFFER -> AnUniformBuffer <$> (do
    pUniformBuffer <- peek @(Ptr DescriptorAddressInfoEXT) (castPtr @_ @(Ptr DescriptorAddressInfoEXT) p)
    maybePeek (\j -> peekCStruct @DescriptorAddressInfoEXT (j)) pUniformBuffer)
  DESCRIPTOR_TYPE_STORAGE_BUFFER -> AStorageBuffer <$> (do
    pStorageBuffer <- peek @(Ptr DescriptorAddressInfoEXT) (castPtr @_ @(Ptr DescriptorAddressInfoEXT) p)
    maybePeek (\j -> peekCStruct @DescriptorAddressInfoEXT (j)) pStorageBuffer)
  DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR -> AnAccelerationStructure <$> (peek @DeviceAddress (castPtr @_ @DeviceAddress p))


type EXT_DESCRIPTOR_BUFFER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_BUFFER_SPEC_VERSION"
pattern EXT_DESCRIPTOR_BUFFER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DESCRIPTOR_BUFFER_SPEC_VERSION = 1


type EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME = "VK_EXT_descriptor_buffer"

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME"
pattern EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME = "VK_EXT_descriptor_buffer"

