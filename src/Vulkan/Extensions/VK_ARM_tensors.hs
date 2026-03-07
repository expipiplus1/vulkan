{-# language CPP #-}
-- | = Name
--
-- VK_ARM_tensors - device extension
--
-- = VK_ARM_tensors
--
-- [__Name String__]
--     @VK_ARM_tensors@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     461
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_descriptor_buffer
--
--     -   Interacts with VK_EXT_frame_boundary
--
--     -   Interacts with VK_EXT_shader_float8
--
--     -   Interacts with VK_KHR_shader_bfloat16
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/ARM/SPV_ARM_tensors.html SPV_ARM_tensors>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_tensors] @kpet%0A*Here describe the issue or question you have about the VK_ARM_tensors extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ARM_tensors.adoc VK_ARM_tensors>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-01-07
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/ARM/SPV_ARM_tensors.html SPV_ARM_tensors>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/arm/GL_ARM_tensors.txt GL_ARM_tensors>
--
--     -   This extension interacts with @VK_EXT_mutable_descriptor_type@
--
--     -   This extension interacts with @VK_EXT_descriptor_buffer@
--
--     -   This extension interacts with @VK_EXT_frame_boundary@
--
--     -   This extension interacts with @VK_EXT_robustness2@
--
--     -   This extension interacts with @VK_KHR_unified_image_layouts@
--
--     -   This extension interacts with @VK_KHR_shader_bfloat16@
--
--     -   This extension interacts with @VK_EXT_shader_float8@
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Einar Hov, Arm Ltd.
--
--     -   Dominic Symes, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Marco Cattani, Arm Ltd.
--
--     -   Lisa Wu, Arm Ltd.
--
--     -   Robert Hughes, Arm Ltd.
--
--     -   David Garbett, Arm Ltd.
--
--     -   Oualid Khelifi, Arm Ltd.
--
-- == Description
--
-- This extension adds support for tensors.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.TensorARM'
--
-- -   'Vulkan.Extensions.Handles.TensorViewARM'
--
-- == New Commands
--
-- -   'bindTensorMemoryARM'
--
-- -   'cmdCopyTensorARM'
--
-- -   'createTensorARM'
--
-- -   'createTensorViewARM'
--
-- -   'destroyTensorARM'
--
-- -   'destroyTensorViewARM'
--
-- -   'getDeviceTensorMemoryRequirementsARM'
--
-- -   'getPhysicalDeviceExternalTensorPropertiesARM'
--
-- -   'getTensorMemoryRequirementsARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   'getTensorOpaqueCaptureDescriptorDataARM'
--
-- -   'getTensorViewOpaqueCaptureDescriptorDataARM'
--
-- == New Structures
--
-- -   'BindTensorMemoryInfoARM'
--
-- -   'CopyTensorInfoARM'
--
-- -   'DeviceTensorMemoryRequirementsARM'
--
-- -   'ExternalTensorPropertiesARM'
--
-- -   'PhysicalDeviceExternalTensorInfoARM'
--
-- -   'TensorCopyARM'
--
-- -   'TensorCreateInfoARM'
--
-- -   'TensorMemoryRequirementsInfoARM'
--
-- -   'TensorViewCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineResourceInfoARM',
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineConstantARM':
--
--     -   'TensorDescriptionARM'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.DependencyInfo':
--
--     -   'TensorDependencyInfoARM'
--
--     -   'TensorMemoryBarrierARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2':
--
--     -   'TensorFormatPropertiesARM'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'MemoryDedicatedAllocateInfoTensorARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTensorFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTensorPropertiesARM'
--
-- -   Extending 'TensorCreateInfoARM':
--
--     -   'ExternalMemoryTensorCreateInfoARM'
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet':
--
--     -   'WriteDescriptorSetTensorARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   'TensorCaptureDescriptorDataInfoARM'
--
-- -   'TensorViewCaptureDescriptorDataInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.DescriptorGetInfoEXT':
--
--     -   'DescriptorGetTensorInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorBufferTensorFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDescriptorBufferTensorPropertiesARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SubmitInfo2',
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
--     'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo':
--
--     -   'FrameBoundaryTensorsARM'
--
-- == New Enums
--
-- -   'TensorCreateFlagBitsARM'
--
-- -   'TensorTilingARM'
--
-- -   'TensorUsageFlagBitsARM'
--
-- -   'TensorViewCreateFlagBitsARM'
--
-- == New Bitmasks
--
-- -   'TensorCreateFlagsARM'
--
-- -   'TensorUsageFlagsARM'
--
-- -   'TensorViewCreateFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_TENSORS_EXTENSION_NAME'
--
-- -   'ARM_TENSORS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R8_BOOL_ARM'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_TENSOR_IMAGE_ALIASING_BIT_ARM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_TENSOR_SHADER_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TENSOR_ALIASING_ARM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TENSOR_ALIASING_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_TENSOR_ARM'
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_TENSOR_VIEW_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_TENSOR_MEMORY_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_TENSOR_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_TENSOR_MEMORY_REQUIREMENTS_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_MEMORY_TENSOR_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_TENSOR_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_TENSOR_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_TENSOR_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_COPY_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_DEPENDENCY_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_DESCRIPTION_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_FORMAT_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_MEMORY_BARRIER_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_MEMORY_REQUIREMENTS_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_VIEW_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_TENSOR_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_GET_TENSOR_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_CAPTURE_DESCRIPTOR_DATA_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_ARM'
--
-- -   Extending 'TensorCreateFlagBitsARM':
--
--     -   'TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM'
--
-- -   Extending 'TensorViewCreateFlagBitsARM':
--
--     -   'TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAME_BOUNDARY_TENSORS_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_float8 VK_EXT_shader_float8>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R8_SFLOAT_FPENCODING_FLOAT8E4M3_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R8_SFLOAT_FPENCODING_FLOAT8E5M2_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_bfloat16 VK_KHR_shader_bfloat16>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R16_SFLOAT_FPENCODING_BFLOAT16_ARM'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-TensorsARM TensorsARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-StorageTensorArrayDynamicIndexingARM StorageTensorArrayDynamicIndexingARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-StorageTensorArrayNonUniformIndexingARM StorageTensorArrayNonUniformIndexingARM>
--
-- == Issues
--
-- 1) Should tensor strides be passed in elements or in bytes?
--
-- __RESOLVED__: Strides are passed in bytes but are required to be a
-- multiple of the tensor element size. Passing strides in bytes makes it
-- possible to relax this requirement in the future without an interface
-- change. It also makes it easier to describe memory alignment
-- requirements.
--
-- 2) Should there be commands to copy data between tensors and
-- buffers\/images?
--
-- __RESOLVED__: Adding these commands would result in a rather large API
-- surface and not insignificant implementation and validation cost. The
-- same outcome can be achieved with memory aliasing and tensor to tensor
-- copy operations.
--
-- 3) Should this extension define transpose and\/or other data
-- reorganization operations?
--
-- __RESOLVED__: These operations are useful to expose but this extension
-- is only meant to add base support for tensors. Additional operations
-- should be layered on top and defined in other extensions.
--
-- 4) Why are tensor strides described using signed integers?
--
-- __RESOLVED__: Negative strides make it possible to describe different
-- linear data layouts. While this extension does not allow negative
-- strides, it uses signed integers for strides to make it possible to
-- relax this limitation in future extensions.
--
-- == Version History
--
-- -   Revision 2, 2026-01-07 (Kévin Petit)
--
--     -   Add interactions with VK_KHR_unified_image_layouts,
--         VK_KHR_shader_bfloat16, and VK_EXT_shader_float8
--
-- -   Revision 1, 2025-06-03 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_tensors Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_tensors  ( createTensorARM
                                         , withTensorARM
                                         , destroyTensorARM
                                         , createTensorViewARM
                                         , withTensorViewARM
                                         , destroyTensorViewARM
                                         , getTensorMemoryRequirementsARM
                                         , bindTensorMemoryARM
                                         , getDeviceTensorMemoryRequirementsARM
                                         , cmdCopyTensorARM
                                         , getTensorOpaqueCaptureDescriptorDataARM
                                         , getTensorViewOpaqueCaptureDescriptorDataARM
                                         , getPhysicalDeviceExternalTensorPropertiesARM
                                         , TensorDescriptionARM(..)
                                         , TensorCreateInfoARM(..)
                                         , TensorViewCreateInfoARM(..)
                                         , TensorMemoryRequirementsInfoARM(..)
                                         , BindTensorMemoryInfoARM(..)
                                         , WriteDescriptorSetTensorARM(..)
                                         , TensorFormatPropertiesARM(..)
                                         , PhysicalDeviceTensorPropertiesARM(..)
                                         , TensorMemoryBarrierARM(..)
                                         , TensorDependencyInfoARM(..)
                                         , PhysicalDeviceTensorFeaturesARM(..)
                                         , DeviceTensorMemoryRequirementsARM(..)
                                         , CopyTensorInfoARM(..)
                                         , TensorCopyARM(..)
                                         , MemoryDedicatedAllocateInfoTensorARM(..)
                                         , PhysicalDeviceDescriptorBufferTensorPropertiesARM(..)
                                         , PhysicalDeviceDescriptorBufferTensorFeaturesARM(..)
                                         , TensorCaptureDescriptorDataInfoARM(..)
                                         , TensorViewCaptureDescriptorDataInfoARM(..)
                                         , DescriptorGetTensorInfoARM(..)
                                         , FrameBoundaryTensorsARM(..)
                                         , PhysicalDeviceExternalTensorInfoARM(..)
                                         , ExternalTensorPropertiesARM(..)
                                         , ExternalMemoryTensorCreateInfoARM(..)
                                         , TensorCreateFlagsARM
                                         , TensorCreateFlagBitsARM( TENSOR_CREATE_MUTABLE_FORMAT_BIT_ARM
                                                                  , TENSOR_CREATE_PROTECTED_BIT_ARM
                                                                  , TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM
                                                                  , TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM
                                                                  , ..
                                                                  )
                                         , TensorUsageFlagsARM
                                         , TensorUsageFlagBitsARM( TENSOR_USAGE_SHADER_BIT_ARM
                                                                 , TENSOR_USAGE_TRANSFER_SRC_BIT_ARM
                                                                 , TENSOR_USAGE_TRANSFER_DST_BIT_ARM
                                                                 , TENSOR_USAGE_IMAGE_ALIASING_BIT_ARM
                                                                 , TENSOR_USAGE_DATA_GRAPH_BIT_ARM
                                                                 , ..
                                                                 )
                                         , TensorTilingARM( TENSOR_TILING_OPTIMAL_ARM
                                                          , TENSOR_TILING_LINEAR_ARM
                                                          , ..
                                                          )
                                         , TensorViewCreateFlagsARM
                                         , TensorViewCreateFlagBitsARM( TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM
                                                                      , ..
                                                                      )
                                         , ARM_TENSORS_SPEC_VERSION
                                         , pattern ARM_TENSORS_SPEC_VERSION
                                         , ARM_TENSORS_EXTENSION_NAME
                                         , pattern ARM_TENSORS_EXTENSION_NAME
                                         , TensorARM(..)
                                         , TensorViewARM(..)
                                         ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
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
import Data.Int (Int32)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkBindTensorMemoryARM))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyTensorARM))
import Vulkan.Dynamic (DeviceCmds(pVkCreateTensorARM))
import Vulkan.Dynamic (DeviceCmds(pVkCreateTensorViewARM))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyTensorARM))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyTensorViewARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceTensorMemoryRequirementsARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetTensorMemoryRequirementsARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetTensorOpaqueCaptureDescriptorDataARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetTensorViewOpaqueCaptureDescriptorDataARM))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalMemoryProperties)
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlags2)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalTensorPropertiesARM))
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_heap (OpaqueCaptureDataCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (OpaqueCaptureDescriptorDataCreateInfoEXT)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (TensorARM)
import Vulkan.Extensions.Handles (TensorARM(..))
import Vulkan.Extensions.Handles (TensorViewARM)
import Vulkan.Extensions.Handles (TensorViewARM(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_TENSOR_MEMORY_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_TENSOR_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_GET_TENSOR_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_TENSOR_MEMORY_REQUIREMENTS_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_TENSOR_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_TENSOR_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAME_BOUNDARY_TENSORS_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_TENSOR_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_TENSOR_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_CAPTURE_DESCRIPTOR_DATA_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_COPY_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_DEPENDENCY_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_DESCRIPTION_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_FORMAT_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_MEMORY_BARRIER_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_MEMORY_REQUIREMENTS_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TENSOR_VIEW_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_TENSOR_ARM))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (TensorARM(..))
import Vulkan.Extensions.Handles (TensorViewARM(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateTensorARM
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct TensorCreateInfoARM) -> Ptr AllocationCallbacks -> Ptr TensorARM -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct TensorCreateInfoARM) -> Ptr AllocationCallbacks -> Ptr TensorARM -> IO Result

-- | vkCreateTensorARM - Create a new tensor object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateTensorARM-tensors-09832# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tensors tensors>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateTensorARM-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateTensorARM-pCreateInfo-parameter# @pCreateInfo@ /must/
--     be a valid pointer to a valid 'TensorCreateInfoARM' structure
--
-- -   #VUID-vkCreateTensorARM-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateTensorARM-pTensor-parameter# @pTensor@ /must/ be a
--     valid pointer to a 'Vulkan.Extensions.Handles.TensorARM' handle
--
-- -   #VUID-vkCreateTensorARM-device-queuecount# The device /must/ have
--     been created with at least @1@ queue
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.TensorARM',
-- 'TensorCreateInfoARM'
createTensorARM :: forall a io
                 . (Extendss TensorCreateInfoARM a, PokeChain a, MonadIO io)
                => -- | @device@ is the logical device that creates the tensor.
                   Device
                -> -- | @pCreateInfo@ is a pointer to a 'TensorCreateInfoARM' structure
                   -- containing parameters to be used to create the tensor.
                   (TensorCreateInfoARM a)
                -> -- | @pAllocator@ controls host memory allocation as described in the
                   -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                   -- chapter.
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io (TensorARM)
createTensorARM device createInfo allocator = liftIO . evalContT $ do
  let vkCreateTensorARMPtr = pVkCreateTensorARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateTensorARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateTensorARM is null" Nothing Nothing
  let vkCreateTensorARM' = mkVkCreateTensorARM vkCreateTensorARMPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPTensor <- ContT $ bracket (callocBytes @TensorARM 8) free
  r <- lift $ traceAroundEvent "vkCreateTensorARM" (vkCreateTensorARM'
                                                      (deviceHandle (device))
                                                      (forgetExtensions pCreateInfo)
                                                      pAllocator
                                                      (pPTensor))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTensor <- lift $ peek @TensorARM pPTensor
  pure $ (pTensor)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createTensorARM' and 'destroyTensorARM'
--
-- To ensure that 'destroyTensorARM' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withTensorARM :: forall a io r . (Extendss TensorCreateInfoARM a, PokeChain a, MonadIO io) => Device -> TensorCreateInfoARM a -> Maybe AllocationCallbacks -> (io TensorARM -> (TensorARM -> io ()) -> r) -> r
withTensorARM device pCreateInfo pAllocator b =
  b (createTensorARM device pCreateInfo pAllocator)
    (\(o0) -> destroyTensorARM device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyTensorARM
  :: FunPtr (Ptr Device_T -> TensorARM -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> TensorARM -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyTensorARM - Destroy a tensor object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyTensorARM-tensor-09730# All submitted commands that
--     refer to @tensor@, either directly or via a
--     'Vulkan.Extensions.Handles.TensorViewARM', /must/ have completed
--     execution
--
-- -   #VUID-vkDestroyTensorARM-tensor-09731# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @tensor@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyTensorARM-tensor-09732# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @tensor@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyTensorARM-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyTensorARM-tensor-parameter# If @tensor@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @tensor@ /must/ be a valid
--     'Vulkan.Extensions.Handles.TensorARM' handle
--
-- -   #VUID-vkDestroyTensorARM-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyTensorARM-tensor-parent# If @tensor@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @tensor@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.TensorARM'
destroyTensorARM :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that destroys the tensor.
                    Device
                 -> -- | @tensor@ is the tensor to destroy.
                    TensorARM
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroyTensorARM device tensor allocator = liftIO . evalContT $ do
  let vkDestroyTensorARMPtr = pVkDestroyTensorARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyTensorARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyTensorARM is null" Nothing Nothing
  let vkDestroyTensorARM' = mkVkDestroyTensorARM vkDestroyTensorARMPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyTensorARM" (vkDestroyTensorARM'
                                                  (deviceHandle (device))
                                                  (tensor)
                                                  pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateTensorViewARM
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct TensorViewCreateInfoARM) -> Ptr AllocationCallbacks -> Ptr TensorViewARM -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct TensorViewCreateInfoARM) -> Ptr AllocationCallbacks -> Ptr TensorViewARM -> IO Result

-- | vkCreateTensorViewARM - Create an tensor view from an existing tensor
--
-- = Description
--
-- Some of the tensor creation parameters are inherited by the view. In
-- particular, other than format, the tensor view creation inherits all
-- other parameters from the tensor.
--
-- The remaining parameters are contained in @pCreateInfo@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateTensorViewARM-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateTensorViewARM-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'TensorViewCreateInfoARM'
--     structure
--
-- -   #VUID-vkCreateTensorViewARM-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateTensorViewARM-pView-parameter# @pView@ /must/ be a
--     valid pointer to a 'Vulkan.Extensions.Handles.TensorViewARM' handle
--
-- -   #VUID-vkCreateTensorViewARM-device-queuecount# The device /must/
--     have been created with at least @1@ queue
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.TensorViewARM', 'TensorViewCreateInfoARM'
createTensorViewARM :: forall a io
                     . ( Extendss TensorViewCreateInfoARM a
                       , PokeChain a
                       , MonadIO io )
                    => -- | @device@ is the logical device that creates the tensor view.
                       Device
                    -> -- | @pCreateInfo@ is a pointer to an instance of the
                       -- 'TensorViewCreateInfoARM' structure containing parameters to be used to
                       -- create the tensor view.
                       (TensorViewCreateInfoARM a)
                    -> -- | @pAllocator@ controls host memory allocation as described in the
                       -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                       -- chapter.
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io (TensorViewARM)
createTensorViewARM device createInfo allocator = liftIO . evalContT $ do
  let vkCreateTensorViewARMPtr = pVkCreateTensorViewARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateTensorViewARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateTensorViewARM is null" Nothing Nothing
  let vkCreateTensorViewARM' = mkVkCreateTensorViewARM vkCreateTensorViewARMPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPView <- ContT $ bracket (callocBytes @TensorViewARM 8) free
  r <- lift $ traceAroundEvent "vkCreateTensorViewARM" (vkCreateTensorViewARM'
                                                          (deviceHandle (device))
                                                          (forgetExtensions pCreateInfo)
                                                          pAllocator
                                                          (pPView))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pView <- lift $ peek @TensorViewARM pPView
  pure $ (pView)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createTensorViewARM' and 'destroyTensorViewARM'
--
-- To ensure that 'destroyTensorViewARM' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withTensorViewARM :: forall a io r . (Extendss TensorViewCreateInfoARM a, PokeChain a, MonadIO io) => Device -> TensorViewCreateInfoARM a -> Maybe AllocationCallbacks -> (io TensorViewARM -> (TensorViewARM -> io ()) -> r) -> r
withTensorViewARM device pCreateInfo pAllocator b =
  b (createTensorViewARM device pCreateInfo pAllocator)
    (\(o0) -> destroyTensorViewARM device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyTensorViewARM
  :: FunPtr (Ptr Device_T -> TensorViewARM -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> TensorViewARM -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyTensorViewARM - Destroy a tensor view object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyTensorViewARM-tensorView-09750# All submitted
--     commands that refer to @tensorView@ /must/ have completed execution
--
-- -   #VUID-vkDestroyTensorViewARM-tensorView-09751# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @tensorView@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyTensorViewARM-tensorView-09752# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @tensorView@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyTensorViewARM-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyTensorViewARM-tensorView-parameter# If @tensorView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @tensorView@ /must/
--     be a valid 'Vulkan.Extensions.Handles.TensorViewARM' handle
--
-- -   #VUID-vkDestroyTensorViewARM-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyTensorViewARM-tensorView-parent# If @tensorView@ is a
--     valid handle, it /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Host Synchronization
--
-- -   Host access to @tensorView@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.TensorViewARM'
destroyTensorViewARM :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the logical device that destroys the tensor view.
                        Device
                     -> -- | @tensorView@ is the tensor view to destroy.
                        TensorViewARM
                     -> -- | @pAllocator@ controls host memory allocation as described in the
                        -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                        -- chapter.
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io ()
destroyTensorViewARM device tensorView allocator = liftIO . evalContT $ do
  let vkDestroyTensorViewARMPtr = pVkDestroyTensorViewARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyTensorViewARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyTensorViewARM is null" Nothing Nothing
  let vkDestroyTensorViewARM' = mkVkDestroyTensorViewARM vkDestroyTensorViewARMPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyTensorViewARM" (vkDestroyTensorViewARM'
                                                      (deviceHandle (device))
                                                      (tensorView)
                                                      pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetTensorMemoryRequirementsARM
  :: FunPtr (Ptr Device_T -> Ptr TensorMemoryRequirementsInfoARM -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr TensorMemoryRequirementsInfoARM -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetTensorMemoryRequirementsARM - Returns the memory requirements for
-- specified Vulkan object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2',
-- 'TensorMemoryRequirementsInfoARM'
getTensorMemoryRequirementsARM :: forall a io
                                . ( Extendss MemoryRequirements2 a
                                  , PokeChain a
                                  , PeekChain a
                                  , MonadIO io )
                               => -- | @device@ is the logical device that owns the tensor.
                                  --
                                  -- #VUID-vkGetTensorMemoryRequirementsARM-device-parameter# @device@ /must/
                                  -- be a valid 'Vulkan.Core10.Handles.Device' handle
                                  Device
                               -> -- | @pInfo@ is a pointer to a 'TensorMemoryRequirementsInfoARM' structure
                                  -- containing parameters required for the memory requirements query.
                                  --
                                  -- #VUID-vkGetTensorMemoryRequirementsARM-pInfo-parameter# @pInfo@ /must/
                                  -- be a valid pointer to a valid 'TensorMemoryRequirementsInfoARM'
                                  -- structure
                                  TensorMemoryRequirementsInfoARM
                               -> io (MemoryRequirements2 a)
getTensorMemoryRequirementsARM device info = liftIO . evalContT $ do
  let vkGetTensorMemoryRequirementsARMPtr = pVkGetTensorMemoryRequirementsARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetTensorMemoryRequirementsARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetTensorMemoryRequirementsARM is null" Nothing Nothing
  let vkGetTensorMemoryRequirementsARM' = mkVkGetTensorMemoryRequirementsARM vkGetTensorMemoryRequirementsARMPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetTensorMemoryRequirementsARM" (vkGetTensorMemoryRequirementsARM'
                                                                (deviceHandle (device))
                                                                pInfo
                                                                (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindTensorMemoryARM
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr BindTensorMemoryInfoARM -> IO Result) -> Ptr Device_T -> Word32 -> Ptr BindTensorMemoryInfoARM -> IO Result

-- | vkBindTensorMemoryARM - Bind device memory to tensor objects
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'BindTensorMemoryInfoARM', 'Vulkan.Core10.Handles.Device'
bindTensorMemoryARM :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the logical device that owns the buffers and memory.
                       --
                       -- #VUID-vkBindTensorMemoryARM-device-parameter# @device@ /must/ be a valid
                       -- 'Vulkan.Core10.Handles.Device' handle
                       Device
                    -> -- | @pBindInfos@ is a pointer to an array of structures of type
                       -- 'BindTensorMemoryInfoARM', describing tensors and memory to bind.
                       --
                       -- #VUID-vkBindTensorMemoryARM-pBindInfos-parameter# @pBindInfos@ /must/ be
                       -- a valid pointer to an array of @bindInfoCount@ valid
                       -- 'BindTensorMemoryInfoARM' structures
                       ("bindInfos" ::: Vector BindTensorMemoryInfoARM)
                    -> io ()
bindTensorMemoryARM device bindInfos = liftIO . evalContT $ do
  let vkBindTensorMemoryARMPtr = pVkBindTensorMemoryARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkBindTensorMemoryARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindTensorMemoryARM is null" Nothing Nothing
  let vkBindTensorMemoryARM' = mkVkBindTensorMemoryARM vkBindTensorMemoryARMPtr
  pPBindInfos <- ContT $ allocaBytes @BindTensorMemoryInfoARM ((Data.Vector.length (bindInfos)) * 40)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBindInfos `plusPtr` (40 * (i)) :: Ptr BindTensorMemoryInfoARM) (e)) (bindInfos)
  r <- lift $ traceAroundEvent "vkBindTensorMemoryARM" (vkBindTensorMemoryARM'
                                                          (deviceHandle (device))
                                                          ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32))
                                                          (pPBindInfos))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceTensorMemoryRequirementsARM
  :: FunPtr (Ptr Device_T -> Ptr DeviceTensorMemoryRequirementsARM -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr DeviceTensorMemoryRequirementsARM -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetDeviceTensorMemoryRequirementsARM - Returns the memory requirements
-- for specified tensor creation infos
--
-- == Valid Usage
--
-- -   #VUID-vkGetDeviceTensorMemoryRequirementsARM-tensors-09831# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tensors tensors>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceTensorMemoryRequirementsARM-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceTensorMemoryRequirementsARM-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'DeviceTensorMemoryRequirementsARM' structure
--
-- -   #VUID-vkGetDeviceTensorMemoryRequirementsARM-pMemoryRequirements-parameter#
--     @pMemoryRequirements@ /must/ be a valid pointer to a
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceTensorMemoryRequirementsARM',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getDeviceTensorMemoryRequirementsARM :: forall a io
                                      . ( Extendss MemoryRequirements2 a
                                        , PokeChain a
                                        , PeekChain a
                                        , MonadIO io )
                                     => -- | @device@ is the logical device intended to own the tensor.
                                        Device
                                     -> -- | @pInfo@ is a pointer to a 'DeviceTensorMemoryRequirementsARM' structure
                                        -- containing parameters required for the memory requirements query.
                                        ("info" ::: DeviceTensorMemoryRequirementsARM)
                                     -> io (MemoryRequirements2 a)
getDeviceTensorMemoryRequirementsARM device info = liftIO . evalContT $ do
  let vkGetDeviceTensorMemoryRequirementsARMPtr = pVkGetDeviceTensorMemoryRequirementsARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceTensorMemoryRequirementsARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceTensorMemoryRequirementsARM is null" Nothing Nothing
  let vkGetDeviceTensorMemoryRequirementsARM' = mkVkGetDeviceTensorMemoryRequirementsARM vkGetDeviceTensorMemoryRequirementsARMPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetDeviceTensorMemoryRequirementsARM" (vkGetDeviceTensorMemoryRequirementsARM'
                                                                      (deviceHandle (device))
                                                                      pInfo
                                                                      (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyTensorARM
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyTensorInfoARM -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyTensorInfoARM -> IO ()

-- | vkCmdCopyTensorARM - Copy data between tensors
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyTensorARM-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyTensorARM-pCopyTensorInfo-parameter#
--     @pCopyTensorInfo@ /must/ be a valid pointer to a valid
--     'CopyTensorInfoARM' structure
--
-- -   #VUID-vkCmdCopyTensorARM-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyTensorARM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
--
-- -   #VUID-vkCmdCopyTensorARM-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyTensorARM-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdCopyTensorARM-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_TRANSFER_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdCopyTensorARM is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyTensorInfoARM'
cmdCopyTensorARM :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is the command buffer into which the command will be
                    -- recorded.
                    CommandBuffer
                 -> -- | @pCopyTensorInfo@ is a pointer to 'CopyTensorInfoARM' structure
                    -- describing the copy parameters.
                    CopyTensorInfoARM
                 -> io ()
cmdCopyTensorARM commandBuffer copyTensorInfo = liftIO . evalContT $ do
  let vkCmdCopyTensorARMPtr = pVkCmdCopyTensorARM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyTensorARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyTensorARM is null" Nothing Nothing
  let vkCmdCopyTensorARM' = mkVkCmdCopyTensorARM vkCmdCopyTensorARMPtr
  pCopyTensorInfo <- ContT $ withCStruct (copyTensorInfo)
  lift $ traceAroundEvent "vkCmdCopyTensorARM" (vkCmdCopyTensorARM'
                                                  (commandBufferHandle (commandBuffer))
                                                  pCopyTensorInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetTensorOpaqueCaptureDescriptorDataARM
  :: FunPtr (Ptr Device_T -> Ptr TensorCaptureDescriptorDataInfoARM -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr TensorCaptureDescriptorDataInfoARM -> Ptr () -> IO Result

-- | vkGetTensorOpaqueCaptureDescriptorDataARM - Get tensor opaque capture
-- descriptor data
--
-- == Valid Usage
--
-- -   #VUID-vkGetTensorOpaqueCaptureDescriptorDataARM-descriptorBufferCaptureReplay-09702#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBuffer descriptorBufferCaptureReplay>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBufferTensorDescriptors descriptorBufferTensorDescriptors>
--     features /must/ be enabled
--
-- -   #VUID-vkGetTensorOpaqueCaptureDescriptorDataARM-pData-09703# @pData@
--     /must/ point to a buffer that is at least
--     'PhysicalDeviceDescriptorBufferTensorPropertiesARM'::@tensorCaptureReplayDescriptorDataSize@
--     bytes in size
--
-- -   #VUID-vkGetTensorOpaqueCaptureDescriptorDataARM-device-09704# If
--     @device@ was created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetTensorOpaqueCaptureDescriptorDataARM-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetTensorOpaqueCaptureDescriptorDataARM-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'TensorCaptureDescriptorDataInfoARM' structure
--
-- -   #VUID-vkGetTensorOpaqueCaptureDescriptorDataARM-pData-parameter#
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Device', 'TensorCaptureDescriptorDataInfoARM'
getTensorOpaqueCaptureDescriptorDataARM :: forall io
                                         . (MonadIO io)
                                        => -- | @device@ is the logical device that gets the data.
                                           Device
                                        -> -- | @pInfo@ is a pointer to a 'TensorCaptureDescriptorDataInfoARM' structure
                                           -- specifying the tensor.
                                           TensorCaptureDescriptorDataInfoARM
                                        -> -- | @pData@ is a pointer to a user-allocated buffer where the data will be
                                           -- written.
                                           ("data" ::: Ptr ())
                                        -> io ()
getTensorOpaqueCaptureDescriptorDataARM device
                                          info
                                          data' = liftIO . evalContT $ do
  let vkGetTensorOpaqueCaptureDescriptorDataARMPtr = pVkGetTensorOpaqueCaptureDescriptorDataARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetTensorOpaqueCaptureDescriptorDataARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetTensorOpaqueCaptureDescriptorDataARM is null" Nothing Nothing
  let vkGetTensorOpaqueCaptureDescriptorDataARM' = mkVkGetTensorOpaqueCaptureDescriptorDataARM vkGetTensorOpaqueCaptureDescriptorDataARMPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetTensorOpaqueCaptureDescriptorDataARM" (vkGetTensorOpaqueCaptureDescriptorDataARM'
                                                                              (deviceHandle (device))
                                                                              pInfo
                                                                              (data'))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetTensorViewOpaqueCaptureDescriptorDataARM
  :: FunPtr (Ptr Device_T -> Ptr TensorViewCaptureDescriptorDataInfoARM -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr TensorViewCaptureDescriptorDataInfoARM -> Ptr () -> IO Result

-- | vkGetTensorViewOpaqueCaptureDescriptorDataARM - Get tensor view opaque
-- capture descriptor data
--
-- == Valid Usage
--
-- -   #VUID-vkGetTensorViewOpaqueCaptureDescriptorDataARM-descriptorBufferCaptureReplay-09706#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBuffer descriptorBufferCaptureReplay>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBufferTensorDescriptors descriptorBufferTensorDescriptors>
--     features /must/ be enabled
--
-- -   #VUID-vkGetTensorViewOpaqueCaptureDescriptorDataARM-pData-09707#
--     @pData@ /must/ point to a buffer that is at least
--     'PhysicalDeviceDescriptorBufferTensorPropertiesARM'::@tensorViewCaptureReplayDescriptorDataSize@
--     bytes in size
--
-- -   #VUID-vkGetTensorViewOpaqueCaptureDescriptorDataARM-device-09708# If
--     @device@ was created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetTensorViewOpaqueCaptureDescriptorDataARM-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetTensorViewOpaqueCaptureDescriptorDataARM-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'TensorViewCaptureDescriptorDataInfoARM' structure
--
-- -   #VUID-vkGetTensorViewOpaqueCaptureDescriptorDataARM-pData-parameter#
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Handles.Device', 'TensorViewCaptureDescriptorDataInfoARM'
getTensorViewOpaqueCaptureDescriptorDataARM :: forall io
                                             . (MonadIO io)
                                            => -- | @device@ is the logical device that gets the data.
                                               Device
                                            -> -- | @pInfo@ is a pointer to a 'TensorViewCaptureDescriptorDataInfoARM'
                                               -- structure specifying the tensor view.
                                               TensorViewCaptureDescriptorDataInfoARM
                                            -> -- | @pData@ is a pointer to a user-allocated buffer where the data will be
                                               -- written.
                                               ("data" ::: Ptr ())
                                            -> io ()
getTensorViewOpaqueCaptureDescriptorDataARM device
                                              info
                                              data' = liftIO . evalContT $ do
  let vkGetTensorViewOpaqueCaptureDescriptorDataARMPtr = pVkGetTensorViewOpaqueCaptureDescriptorDataARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetTensorViewOpaqueCaptureDescriptorDataARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetTensorViewOpaqueCaptureDescriptorDataARM is null" Nothing Nothing
  let vkGetTensorViewOpaqueCaptureDescriptorDataARM' = mkVkGetTensorViewOpaqueCaptureDescriptorDataARM vkGetTensorViewOpaqueCaptureDescriptorDataARMPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetTensorViewOpaqueCaptureDescriptorDataARM" (vkGetTensorViewOpaqueCaptureDescriptorDataARM'
                                                                                  (deviceHandle (device))
                                                                                  pInfo
                                                                                  (data'))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalTensorPropertiesARM
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceExternalTensorInfoARM -> Ptr ExternalTensorPropertiesARM -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceExternalTensorInfoARM -> Ptr ExternalTensorPropertiesARM -> IO ()

-- | vkGetPhysicalDeviceExternalTensorPropertiesARM - Function for querying
-- external tensor handle capabilities.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'ExternalTensorPropertiesARM', 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceExternalTensorInfoARM'
getPhysicalDeviceExternalTensorPropertiesARM :: forall io
                                              . (MonadIO io)
                                             => -- | @physicalDevice@ is the physical device from which to query the tensor
                                                -- capabilities.
                                                --
                                                -- #VUID-vkGetPhysicalDeviceExternalTensorPropertiesARM-physicalDevice-parameter#
                                                -- @physicalDevice@ /must/ be a valid
                                                -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                PhysicalDevice
                                             -> -- | @pExternalTensorInfo@ is a pointer to a
                                                -- 'PhysicalDeviceExternalTensorInfoARM' structure describing the
                                                -- parameters that would be consumed by 'createTensorARM'.
                                                --
                                                -- #VUID-vkGetPhysicalDeviceExternalTensorPropertiesARM-pExternalTensorInfo-parameter#
                                                -- @pExternalTensorInfo@ /must/ be a valid pointer to a valid
                                                -- 'PhysicalDeviceExternalTensorInfoARM' structure
                                                PhysicalDeviceExternalTensorInfoARM
                                             -> io (ExternalTensorPropertiesARM)
getPhysicalDeviceExternalTensorPropertiesARM physicalDevice
                                               externalTensorInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalTensorPropertiesARMPtr = pVkGetPhysicalDeviceExternalTensorPropertiesARM (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceExternalTensorPropertiesARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceExternalTensorPropertiesARM is null" Nothing Nothing
  let vkGetPhysicalDeviceExternalTensorPropertiesARM' = mkVkGetPhysicalDeviceExternalTensorPropertiesARM vkGetPhysicalDeviceExternalTensorPropertiesARMPtr
  pExternalTensorInfo <- ContT $ withCStruct (externalTensorInfo)
  pPExternalTensorProperties <- ContT (withZeroCStruct @ExternalTensorPropertiesARM)
  lift $ traceAroundEvent "vkGetPhysicalDeviceExternalTensorPropertiesARM" (vkGetPhysicalDeviceExternalTensorPropertiesARM'
                                                                              (physicalDeviceHandle (physicalDevice))
                                                                              pExternalTensorInfo
                                                                              (pPExternalTensorProperties))
  pExternalTensorProperties <- lift $ peekCStruct @ExternalTensorPropertiesARM pPExternalTensorProperties
  pure $ (pExternalTensorProperties)


-- | VkTensorDescriptionARM - Structure describing a tensor
--
-- = Description
--
-- When describing a tensor created with 'TENSOR_TILING_OPTIMAL_ARM',
-- @pStrides@ must be equal to @NULL@. When describing a tensor created
-- with 'TENSOR_TILING_LINEAR_ARM', @pStrides@ is either an array of size
-- @dimensionCount@ or @NULL@.
--
-- The formats that /must/ be supported for @format@ are documented in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-formats-mandatory-features-tensor>.
--
-- Each element in the @pStrides@ array describes the offset in bytes
-- between increments of the given dimension. For example, @pStrides@[0]
-- describes the offset between element [x0,x1,x2,x3] and element
-- [x0+1,x1,x2,x3]. The @pStrides@ array /can/ be used to determine whether
-- a tensor is /packed/ or not. If @pStrides@[@dimensionCount@-1] is equal
-- to the size of a tensor element and for each dimension @n@ greater than
-- 0 and less than @dimensionCount@, @pStrides@[n-1] is equal to
-- @pStrides@[n] * @pDimensions@[n], then the tensor is a packed tensor. If
-- the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tensorNonPacked tensorNonPacked>
-- feature is not enabled, the tensor /must/ be a packed tensor.
--
-- When a tensor is created with 'TENSOR_TILING_LINEAR_ARM' and @pStrides@
-- equal to @NULL@ the tensor strides are calculated by the vulkan
-- implementation such that the resulting tensor is a packed tensor.
--
-- Expressed as an addressing formula, the starting byte of an element in a
-- 4-dimensional, for example, linear tensor has address:
--
-- > // Assume (x0,x1,x2,x3) are in units of elements.
-- >
-- > address(x0,x1,x2,x3) = x0*pStrides[0] + x1*pStrides[1] + x2*pStrides[2] + x3*pStrides[3]
--
-- == Valid Usage
--
-- -   #VUID-VkTensorDescriptionARM-dimensionCount-09733# @dimensionCount@
--     /must/ be less than or equal to
--     'PhysicalDeviceTensorPropertiesARM'::@maxTensorDimensionCount@
--
-- -   #VUID-VkTensorDescriptionARM-pDimensions-09734# For each i where i ≤
--     dimensionCount-1, @pDimensions@[i] /must/ be greater than @0@
--
-- -   #VUID-VkTensorDescriptionARM-pDimensions-09883# For each i where i ≤
--     dimensionCount-1, @pDimensions@[i] /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxPerDimensionTensorElements ::maxPerDimensionTensorElements>
--
-- -   #VUID-VkTensorDescriptionARM-format-09735# @format@ /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' and /must/ be a
--     one-component 'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-VkTensorDescriptionARM-pStrides-09736#
--     @pStrides@[@dimensionCount@-1] /must/ equal the size in bytes of a
--     tensor element
--
-- -   #VUID-VkTensorDescriptionARM-pStrides-09737# For each i,
--     @pStrides@[i] /must/ be a multiple of the element size
--
-- -   #VUID-VkTensorDescriptionARM-pStrides-09738# For each i,
--     @pStrides@[i] /must/ be greater than @0@ and less than or equal to
--     'PhysicalDeviceTensorPropertiesARM'::@maxTensorStride@
--
-- -   #VUID-VkTensorDescriptionARM-pStrides-09884# @pStrides@[0] ×
--     @pDimensions@[0] /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxTensorSize ::maxTensorSize>
--
-- -   #VUID-VkTensorDescriptionARM-pStrides-09739# For each i greater than
--     0, @pStrides@[i-1] /must/ be greater than or equal to @pStrides@[i]
--     × @pDimensions@[i] so that no two elements of the tensor reference
--     the same memory address
--
-- -   #VUID-VkTensorDescriptionARM-None-09740# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tensorNonPacked tensorNonPacked>
--     feature is not enabled, then the members of 'TensorDescriptionARM'
--     /must/ describe a packed tensor
--
-- -   #VUID-VkTensorDescriptionARM-tiling-09741# If @tiling@ is
--     'TENSOR_TILING_OPTIMAL_ARM' and @usage@ is
--     'TENSOR_USAGE_IMAGE_ALIASING_BIT_ARM' then the size of the tensor
--     along its innermost dimension, i.e. @pDimensions@[@dimensionCount@ -
--     1], /must/ be less than or equal to @4@
--
-- -   #VUID-VkTensorDescriptionARM-tiling-09742# If @tiling@ is
--     'TENSOR_TILING_LINEAR_ARM' then
--     'TENSOR_USAGE_IMAGE_ALIASING_BIT_ARM' /must/ not be set in @usage@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkTensorDescriptionARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_DESCRIPTION_ARM'
--
-- -   #VUID-VkTensorDescriptionARM-tiling-parameter# @tiling@ /must/ be a
--     valid 'TensorTilingARM' value
--
-- -   #VUID-VkTensorDescriptionARM-format-parameter# @format@ /must/ be a
--     valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkTensorDescriptionARM-pDimensions-parameter# @pDimensions@
--     /must/ be a valid pointer to an array of @dimensionCount@ @int64_t@
--     values
--
-- -   #VUID-VkTensorDescriptionARM-pStrides-parameter# If @pStrides@ is
--     not @NULL@, @pStrides@ /must/ be a valid pointer to an array of
--     @dimensionCount@ @int64_t@ values
--
-- -   #VUID-VkTensorDescriptionARM-usage-parameter# @usage@ /must/ be a
--     valid combination of 'TensorUsageFlagBitsARM' values
--
-- -   #VUID-VkTensorDescriptionARM-usage-requiredbitmask# @usage@ /must/
--     not be @0@
--
-- -   #VUID-VkTensorDescriptionARM-dimensionCount-arraylength#
--     @dimensionCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'PhysicalDeviceExternalTensorInfoARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'TensorCreateInfoARM', 'TensorTilingARM', 'TensorUsageFlagsARM'
data TensorDescriptionARM = TensorDescriptionARM
  { -- | @tiling@ is a 'TensorTilingARM' value specifying the tiling of the
    -- tensor
    tiling :: TensorTilingARM
  , -- | @format@ is a one component 'Vulkan.Core10.Enums.Format.Format'
    -- describing the format and type of the data elements that will be
    -- contained in the tensor.
    format :: Format
  , -- | @pDimensions@ is a pointer to an array of integers of size
    -- @dimensionCount@ providing the number of data elements in each
    -- dimension.
    dimensions :: Vector Int64
  , -- | @pStrides@ is either @NULL@ or is an array of size @dimensionCount@
    -- providing the strides in bytes for the tensor in each dimension.
    strides :: Vector Int64
  , -- | @usage@ is a bitmask of 'TensorUsageFlagBitsARM' specifying the usage of
    -- the tensor.
    usage :: TensorUsageFlagsARM
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorDescriptionARM)
#endif
deriving instance Show TensorDescriptionARM

instance ToCStruct TensorDescriptionARM where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorDescriptionARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_DESCRIPTION_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr TensorTilingARM)) (tiling)
    lift $ poke ((p `plusPtr` 20 :: Ptr Format)) (format)
    let pDimensionsLength = Data.Vector.length $ (dimensions)
    let pStridesLength = Data.Vector.length $ (strides)
    lift $ unless (fromIntegral pStridesLength == pDimensionsLength || pStridesLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pStrides and pDimensions must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral pDimensionsLength :: Word32))
    pPDimensions' <- ContT $ allocaBytes @Int64 ((Data.Vector.length (dimensions)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDimensions' `plusPtr` (8 * (i)) :: Ptr Int64) (e)) (dimensions)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Int64))) (pPDimensions')
    pStrides'' <- if Data.Vector.null (strides)
      then pure nullPtr
      else do
        pPStrides <- ContT $ allocaBytes @Int64 (((Data.Vector.length (strides))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPStrides `plusPtr` (8 * (i)) :: Ptr Int64) (e)) ((strides))
        pure $ pPStrides
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Int64))) pStrides''
    lift $ poke ((p `plusPtr` 48 :: Ptr TensorUsageFlagsARM)) (usage)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_DESCRIPTION_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorTilingARM)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 48 :: Ptr TensorUsageFlagsARM)) (zero)
    f

instance FromCStruct TensorDescriptionARM where
  peekCStruct p = do
    tiling <- peek @TensorTilingARM ((p `plusPtr` 16 :: Ptr TensorTilingARM))
    format <- peek @Format ((p `plusPtr` 20 :: Ptr Format))
    dimensionCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pDimensions <- peek @(Ptr Int64) ((p `plusPtr` 32 :: Ptr (Ptr Int64)))
    pDimensions' <- generateM (fromIntegral dimensionCount) (\i -> peek @Int64 ((pDimensions `advancePtrBytes` (8 * (i)) :: Ptr Int64)))
    pStrides <- peek @(Ptr Int64) ((p `plusPtr` 40 :: Ptr (Ptr Int64)))
    let pStridesLength = if pStrides == nullPtr then 0 else (fromIntegral dimensionCount)
    pStrides' <- generateM pStridesLength (\i -> peek @Int64 ((pStrides `advancePtrBytes` (8 * (i)) :: Ptr Int64)))
    usage <- peek @TensorUsageFlagsARM ((p `plusPtr` 48 :: Ptr TensorUsageFlagsARM))
    pure $ TensorDescriptionARM
             tiling format pDimensions' pStrides' usage

instance Zero TensorDescriptionARM where
  zero = TensorDescriptionARM
           zero
           zero
           mempty
           mempty
           zero


-- No documentation found for TopLevel "VkTensorCreateInfoARM"
data TensorCreateInfoARM (es :: [Type]) = TensorCreateInfoARM
  { -- No documentation found for Nested "VkTensorCreateInfoARM" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkTensorCreateInfoARM" "flags"
    flags :: TensorCreateFlagsARM
  , -- No documentation found for Nested "VkTensorCreateInfoARM" "pDescription"
    description :: TensorDescriptionARM
  , -- No documentation found for Nested "VkTensorCreateInfoARM" "sharingMode"
    sharingMode :: SharingMode
  , -- No documentation found for Nested "VkTensorCreateInfoARM" "pQueueFamilyIndices"
    queueFamilyIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorCreateInfoARM (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (TensorCreateInfoARM es)

instance Extensible TensorCreateInfoARM where
  extensibleTypeName = "TensorCreateInfoARM"
  setNext TensorCreateInfoARM{..} next' = TensorCreateInfoARM{next = next', ..}
  getNext TensorCreateInfoARM{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends TensorCreateInfoARM e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @OpaqueCaptureDataCreateInfoEXT = Just f
    | Just Refl <- eqT @e @ExternalMemoryTensorCreateInfoARM = Just f
    | Just Refl <- eqT @e @OpaqueCaptureDescriptorDataCreateInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss TensorCreateInfoARM es
         , PokeChain es ) => ToCStruct (TensorCreateInfoARM es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorCreateInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_CREATE_INFO_ARM)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr TensorCreateFlagsARM)) (flags)
    pDescription'' <- ContT $ withCStruct (description)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr TensorDescriptionARM))) pDescription''
    lift $ poke ((p `plusPtr` 32 :: Ptr SharingMode)) (sharingMode)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_CREATE_INFO_ARM)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pDescription'' <- ContT $ withCStruct (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr TensorDescriptionARM))) pDescription''
    lift $ poke ((p `plusPtr` 32 :: Ptr SharingMode)) (zero)
    lift $ f

instance ( Extendss TensorCreateInfoARM es
         , PeekChain es ) => FromCStruct (TensorCreateInfoARM es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @TensorCreateFlagsARM ((p `plusPtr` 16 :: Ptr TensorCreateFlagsARM))
    pDescription <- peekCStruct @TensorDescriptionARM =<< peek ((p `plusPtr` 24 :: Ptr (Ptr TensorDescriptionARM)))
    sharingMode <- peek @SharingMode ((p `plusPtr` 32 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ TensorCreateInfoARM
             next flags pDescription sharingMode pQueueFamilyIndices'

instance es ~ '[] => Zero (TensorCreateInfoARM es) where
  zero = TensorCreateInfoARM
           ()
           zero
           zero
           zero
           mempty


-- | VkTensorViewCreateInfoARM - Structure specifying parameters of a newly
-- created tensor view
--
-- = Description
--
-- If @tensor@ was created with the 'TENSOR_CREATE_MUTABLE_FORMAT_BIT_ARM'
-- flag, @format@ /can/ be different from the tensor’s format, but if they
-- are not equal they /must/ be /compatible/. Tensor format compatibility
-- is defined in the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
-- section. Views of compatible formats will have the same mapping between
-- element locations irrespective of the @format@, with only the
-- interpretation of the bit pattern changing.
--
-- Values intended to be used with one view format /may/ not be exactly
-- preserved when written or read through a different format. For example,
-- an integer value that happens to have the bit pattern of a
-- floating-point denorm or NaN /may/ be flushed or canonicalized when
-- written or read through a view with a floating-point format. Similarly,
-- a value written through a signed normalized format that has a bit
-- pattern exactly equal to -2b /may/ be changed to -2b + 1 as described in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fundamentals-fixedfpconv Conversion from Normalized Fixed-Point to Floating-Point>.
--
-- == Valid Usage
--
-- -   #VUID-VkTensorViewCreateInfoARM-tensor-09743# If @tensor@ was not
--     created with 'TENSOR_CREATE_MUTABLE_FORMAT_BIT_ARM' flag, @format@
--     /must/ be identical to the @format@ used to create @tensor@
--
-- -   #VUID-VkTensorViewCreateInfoARM-tensor-09744# If @tensor@ was
--     created with 'TENSOR_CREATE_MUTABLE_FORMAT_BIT_ARM' flag, @format@
--     /must/ be compatible with the @format@ used to create @tensor@, as
--     defined in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
--
-- -   #VUID-VkTensorViewCreateInfoARM-flags-09745# If @flags@ includes
--     'TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM', the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBufferCaptureReplay descriptorBufferCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-VkTensorViewCreateInfoARM-pNext-09746# If the @pNext@ chain
--     includes a
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
--     structure, @flags@ /must/ contain
--     'TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM'
--
-- -   #VUID-VkTensorViewCreateInfoARM-usage-09747# The @usage@ flags of
--     @tensor@ /must/ have at least one of the following bits set:
--
--     -   'TENSOR_USAGE_SHADER_BIT_ARM'
--
--     -   'TENSOR_USAGE_DATA_GRAPH_BIT_ARM'
--
-- -   #VUID-VkTensorViewCreateInfoARM-usage-09748# The tensor view’s
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-tensor-view-format-features format features>
--     /must/ contain the format feature flags required by the @usage@
--     flags of @tensor@ for @format@ as indicated in the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#format-feature-dependent-usage-flags>
--     section
--
-- -   #VUID-VkTensorViewCreateInfoARM-tensor-09749# If @tensor@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkTensorViewCreateInfoARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_VIEW_CREATE_INFO_ARM'
--
-- -   #VUID-VkTensorViewCreateInfoARM-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
--
-- -   #VUID-VkTensorViewCreateInfoARM-sType-unique# The @sType@ value of
--     each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkTensorViewCreateInfoARM-flags-parameter# @flags@ /must/ be a
--     valid combination of 'TensorViewCreateFlagBitsARM' values
--
-- -   #VUID-VkTensorViewCreateInfoARM-tensor-parameter# @tensor@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.TensorARM' handle
--
-- -   #VUID-VkTensorViewCreateInfoARM-format-parameter# @format@ /must/ be
--     a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.ResourceDescriptorDataEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorARM', 'TensorViewCreateFlagsARM',
-- 'createTensorViewARM'
data TensorViewCreateInfoARM (es :: [Type]) = TensorViewCreateInfoARM
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: TensorViewCreateFlagsARM
  , -- | @tensor@ is a 'Vulkan.Extensions.Handles.TensorARM' on which the view
    -- will be created.
    tensor :: TensorARM
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' describing the format
    -- and type used to interpret elements in the tensor.
    format :: Format
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorViewCreateInfoARM (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (TensorViewCreateInfoARM es)

instance Extensible TensorViewCreateInfoARM where
  extensibleTypeName = "TensorViewCreateInfoARM"
  setNext TensorViewCreateInfoARM{..} next' = TensorViewCreateInfoARM{next = next', ..}
  getNext TensorViewCreateInfoARM{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends TensorViewCreateInfoARM e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @OpaqueCaptureDescriptorDataCreateInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss TensorViewCreateInfoARM es
         , PokeChain es ) => ToCStruct (TensorViewCreateInfoARM es) where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorViewCreateInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_VIEW_CREATE_INFO_ARM)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr TensorViewCreateFlagsARM)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr TensorARM)) (tensor)
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (format)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_VIEW_CREATE_INFO_ARM)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr TensorARM)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    lift $ f

instance ( Extendss TensorViewCreateInfoARM es
         , PeekChain es ) => FromCStruct (TensorViewCreateInfoARM es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @TensorViewCreateFlagsARM ((p `plusPtr` 16 :: Ptr TensorViewCreateFlagsARM))
    tensor <- peek @TensorARM ((p `plusPtr` 24 :: Ptr TensorARM))
    format <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    pure $ TensorViewCreateInfoARM
             next flags tensor format

instance es ~ '[] => Zero (TensorViewCreateInfoARM es) where
  zero = TensorViewCreateInfoARM
           ()
           zero
           zero
           zero


-- | VkTensorMemoryRequirementsInfoARM - Structure specifying memory
-- requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorARM', 'getTensorMemoryRequirementsARM'
data TensorMemoryRequirementsInfoARM = TensorMemoryRequirementsInfoARM
  { -- | @tensor@ is the tensor to query.
    --
    -- #VUID-VkTensorMemoryRequirementsInfoARM-tensor-parameter# @tensor@
    -- /must/ be a valid 'Vulkan.Extensions.Handles.TensorARM' handle
    tensor :: TensorARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorMemoryRequirementsInfoARM)
#endif
deriving instance Show TensorMemoryRequirementsInfoARM

instance ToCStruct TensorMemoryRequirementsInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorMemoryRequirementsInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_MEMORY_REQUIREMENTS_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (tensor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_MEMORY_REQUIREMENTS_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (zero)
    f

instance FromCStruct TensorMemoryRequirementsInfoARM where
  peekCStruct p = do
    tensor <- peek @TensorARM ((p `plusPtr` 16 :: Ptr TensorARM))
    pure $ TensorMemoryRequirementsInfoARM
             tensor

instance Storable TensorMemoryRequirementsInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TensorMemoryRequirementsInfoARM where
  zero = TensorMemoryRequirementsInfoARM
           zero


-- | VkBindTensorMemoryInfoARM - Structure specifying how to bind a tensor to
-- memory
--
-- == Valid Usage
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-09712# @tensor@ /must/ not
--     already be backed by a memory object
--
-- -   #VUID-VkBindTensorMemoryInfoARM-memoryOffset-09713# @memoryOffset@
--     /must/ be less than the size of @memory@
--
-- -   #VUID-VkBindTensorMemoryInfoARM-memory-09714# @memory@ /must/ have
--     been allocated using one of the memory types allowed in the
--     @memoryTypeBits@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to 'getTensorMemoryRequirementsARM' with
--     @tensor@
--
-- -   #VUID-VkBindTensorMemoryInfoARM-memoryOffset-09715# @memoryOffset@
--     /must/ be an integer multiple of the @alignment@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to 'getTensorMemoryRequirementsARM' with
--     @tensor@
--
-- -   #VUID-VkBindTensorMemoryInfoARM-size-09716# The @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to 'getTensorMemoryRequirementsARM' with
--     @tensor@ /must/ be less than or equal to the size of @memory@ minus
--     @memoryOffset@
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-09717# If @tensor@ requires a
--     dedicated allocation (as reported by
--     'getTensorMemoryRequirementsARM' in
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements'::@requiresDedicatedAllocation@
--     for @tensor@), @memory@ /must/ have been created with
--     'MemoryDedicatedAllocateInfoTensorARM'::@tensor@ equal to @tensor@
--
-- -   #VUID-VkBindTensorMemoryInfoARM-memory-09806# If the
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' provided when @memory@ was
--     allocated included a 'MemoryDedicatedAllocateInfoTensorARM'
--     structure in its @pNext@ chain, and
--     'MemoryDedicatedAllocateInfoTensorARM'::@tensor@ was not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', then @tensor@ /must/ equal
--     'MemoryDedicatedAllocateInfoTensorARM'::@tensor@, and @memoryOffset@
--     /must/ be zero
--
-- -   #VUID-VkBindTensorMemoryInfoARM-memory-09895# If the value of
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     used to allocate @memory@ is not @0@, it /must/ include at least one
--     of the handles set in
--     'ExternalMemoryTensorCreateInfoARM'::@handleTypes@ when @tensor@ was
--     created
--
-- -   #VUID-VkBindTensorMemoryInfoARM-memory-09896# If @memory@ was
--     allocated by a memory import operation, that is not
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ImportAndroidHardwareBufferInfoANDROID'
--     with a non-@NULL@ @buffer@ value, the external handle type of the
--     imported memory /must/ also have been set in
--     'ExternalMemoryTensorCreateInfoARM'::@handleTypes@ when @tensor@ was
--     created
--
-- -   #VUID-VkBindTensorMemoryInfoARM-memory-09897# If @memory@ was
--     allocated with the
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ImportAndroidHardwareBufferInfoANDROID'
--     memory import operation with a non-@NULL@ @buffer@ value,
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     /must/ also have been set in
--     'ExternalMemoryTensorCreateInfoARM'::@handleTypes@ when @tensor@ was
--     created
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-09718# If @tensor@ was
--     created with the 'TENSOR_CREATE_PROTECTED_BIT_ARM' bit set, the
--     tensor /must/ be bound to a memory object allocated with a memory
--     type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-09719# If @tensor@ was
--     created with the 'TENSOR_CREATE_PROTECTED_BIT_ARM' bit not set, the
--     tensor /must/ not be bound to a memory object allocated with a
--     memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-09943# If @tensor@ was
--     created with the
--     'TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM' bit set,
--     @memory@ /must/ have been allocated with the
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT'
--     bit set
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-09944# If @tensor@ was
--     created with the
--     'TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM' bit set,
--     @memory@ /must/ have been allocated with the
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--     bit set
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-11406# If @tensor@ was
--     created with the
--     'TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM' bit set,
--     @memory@ /must/ have been allocated with the
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT'
--     bit set
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-11407# If @tensor@ was
--     created with the
--     'TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM' bit set,
--     @memory@ /must/ have been allocated with the
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--     bit set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindTensorMemoryInfoARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_TENSOR_MEMORY_INFO_ARM'
--
-- -   #VUID-VkBindTensorMemoryInfoARM-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkBindTensorMemoryInfoARM-tensor-parameter# @tensor@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.TensorARM' handle
--
-- -   #VUID-VkBindTensorMemoryInfoARM-memory-parameter# @memory@ /must/ be
--     a valid 'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   #VUID-VkBindTensorMemoryInfoARM-commonparent# Both of @memory@, and
--     @tensor@ /must/ have been created, allocated, or retrieved from the
--     same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @tensor@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorARM', 'bindTensorMemoryARM'
data BindTensorMemoryInfoARM = BindTensorMemoryInfoARM
  { -- | @tensor@ is the tensor to be attached to memory.
    tensor :: TensorARM
  , -- | @memory@ is a 'Vulkan.Core10.Handles.DeviceMemory' object describing the
    -- device memory to attach.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of @memory@ which is to
    -- be bound to the tensor. The number of bytes returned in the
    -- 'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ member in
    -- @memory@, starting from @memoryOffset@ bytes, will be bound to the
    -- specified tensor.
    memoryOffset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindTensorMemoryInfoARM)
#endif
deriving instance Show BindTensorMemoryInfoARM

instance ToCStruct BindTensorMemoryInfoARM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindTensorMemoryInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_TENSOR_MEMORY_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (tensor)
    poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (memoryOffset)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_TENSOR_MEMORY_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BindTensorMemoryInfoARM where
  peekCStruct p = do
    tensor <- peek @TensorARM ((p `plusPtr` 16 :: Ptr TensorARM))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ BindTensorMemoryInfoARM
             tensor memory memoryOffset

instance Storable BindTensorMemoryInfoARM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindTensorMemoryInfoARM where
  zero = BindTensorMemoryInfoARM
           zero
           zero
           zero


-- | VkWriteDescriptorSetTensorARM - Structure specifying descriptor tensor
-- info
--
-- == Valid Usage
--
-- -   #VUID-VkWriteDescriptorSetTensorARM-nullDescriptor-09898# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, each element of @pTensorViews@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkWriteDescriptorSetTensorARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_TENSOR_ARM'
--
-- -   #VUID-VkWriteDescriptorSetTensorARM-pTensorViews-parameter#
--     @pTensorViews@ /must/ be a valid pointer to an array of
--     @tensorViewCount@ valid or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Extensions.Handles.TensorViewARM' handles
--
-- -   #VUID-VkWriteDescriptorSetTensorARM-tensorViewCount-arraylength#
--     @tensorViewCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorViewARM'
data WriteDescriptorSetTensorARM = WriteDescriptorSetTensorARM
  { -- | @pTensorViews@ are the tensor views that will be used to update the
    -- descriptor set.
    tensorViews :: Vector TensorViewARM }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSetTensorARM)
#endif
deriving instance Show WriteDescriptorSetTensorARM

instance ToCStruct WriteDescriptorSetTensorARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetTensorARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_TENSOR_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (tensorViews)) :: Word32))
    pPTensorViews' <- ContT $ allocaBytes @TensorViewARM ((Data.Vector.length (tensorViews)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPTensorViews' `plusPtr` (8 * (i)) :: Ptr TensorViewARM) (e)) (tensorViews)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr TensorViewARM))) (pPTensorViews')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_TENSOR_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct WriteDescriptorSetTensorARM where
  peekCStruct p = do
    tensorViewCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pTensorViews <- peek @(Ptr TensorViewARM) ((p `plusPtr` 24 :: Ptr (Ptr TensorViewARM)))
    pTensorViews' <- generateM (fromIntegral tensorViewCount) (\i -> peek @TensorViewARM ((pTensorViews `advancePtrBytes` (8 * (i)) :: Ptr TensorViewARM)))
    pure $ WriteDescriptorSetTensorARM
             pTensorViews'

instance Zero WriteDescriptorSetTensorARM where
  zero = WriteDescriptorSetTensorARM
           mempty


-- | VkTensorFormatPropertiesARM - Structure specifying properties of a
-- format used to describe tensor elements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data TensorFormatPropertiesARM = TensorFormatPropertiesARM
  { -- | @optimalTilingTensorFeatures@ is a bitmask of
    -- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2'
    -- specifying features supported by tensors created with a @tiling@
    -- parameter of 'TENSOR_TILING_OPTIMAL_ARM'.
    optimalTilingTensorFeatures :: FormatFeatureFlags2
  , -- | @linearTilingTensorFeatures@ is a bitmask of
    -- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2'
    -- specifying features supported by tensors created with a @tiling@
    -- parameter of 'TENSOR_TILING_LINEAR_ARM'.
    linearTilingTensorFeatures :: FormatFeatureFlags2
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorFormatPropertiesARM)
#endif
deriving instance Show TensorFormatPropertiesARM

instance ToCStruct TensorFormatPropertiesARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorFormatPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_FORMAT_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FormatFeatureFlags2)) (optimalTilingTensorFeatures)
    poke ((p `plusPtr` 24 :: Ptr FormatFeatureFlags2)) (linearTilingTensorFeatures)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_FORMAT_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FormatFeatureFlags2)) (zero)
    poke ((p `plusPtr` 24 :: Ptr FormatFeatureFlags2)) (zero)
    f

instance FromCStruct TensorFormatPropertiesARM where
  peekCStruct p = do
    optimalTilingTensorFeatures <- peek @FormatFeatureFlags2 ((p `plusPtr` 16 :: Ptr FormatFeatureFlags2))
    linearTilingTensorFeatures <- peek @FormatFeatureFlags2 ((p `plusPtr` 24 :: Ptr FormatFeatureFlags2))
    pure $ TensorFormatPropertiesARM
             optimalTilingTensorFeatures linearTilingTensorFeatures

instance Storable TensorFormatPropertiesARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TensorFormatPropertiesARM where
  zero = TensorFormatPropertiesARM
           zero
           zero


-- | VkPhysicalDeviceTensorPropertiesARM - Structure describing the tensor
-- properties of a physical device
--
-- = Description
--
-- If the 'PhysicalDeviceTensorPropertiesARM' structure is included in the
-- @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTensorPropertiesARM = PhysicalDeviceTensorPropertiesARM
  { -- | #limits-maxTensorDimensionCount# @maxTensorDimensionCount@ is the
    -- maximum number of dimensions that can be specified in the
    -- @dimensionCount@ member of 'TensorDescriptionARM'.
    maxTensorDimensionCount :: Word32
  , -- | #limits-maxTensorElements# @maxTensorElements@ is the maximum number of
    -- data elements in a created tensor as specified in the
    -- 'TensorDescriptionARM' of 'TensorCreateInfoARM'. The number of data
    -- elements in a tensor is computed as the product of @pDimensions@[i] for
    -- all 0 ≤ i ≤ dimensionCount-1.
    maxTensorElements :: Word64
  , -- | #limits-maxPerDimensionTensorElements# @maxPerDimensionTensorElements@
    -- is the maximum number of data elements alongside any dimension of a
    -- tensor.
    maxPerDimensionTensorElements :: Word64
  , -- | #limits-maxTensorStride# @maxTensorStride@ is the maximum value for a
    -- tensor stride that can be used in 'TensorDescriptionARM'::@pStrides@.
    maxTensorStride :: Int64
  , -- | #limits-maxTensorSize# @maxTensorSize@ is the maximum size, in bytes, of
    -- a tensor.
    maxTensorSize :: Word64
  , -- | #limits-maxTensorShaderAccessArrayLength#
    -- @maxTensorShaderAccessArrayLength@ is the maximum number of elements in
    -- an array returned by @OpTensoReadARM@ or consumed by @OpTensorWriteARM@.
    maxTensorShaderAccessArrayLength :: Word32
  , -- | #limits-maxTensorShaderAccessSize# @maxTensorShaderAccessSize@ is the
    -- maximum size in bytes of the data that can be read from a tensor with
    -- @OpTensorReadARM@ or written to a tensor with @OpTensorWriteARM@.
    maxTensorShaderAccessSize :: Word32
  , -- | #limits-maxDescriptorSetStorageTensors# @maxDescriptorSetStorageTensors@
    -- is the maximum number of tensors that /can/ be included in descriptor
    -- bindings in a pipeline layout across all pipeline shader stages and
    -- descriptor set numbers. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM' count
    -- against this limit.
    maxDescriptorSetStorageTensors :: Word32
  , -- | #limits-maxPerStageDescriptorSetStorageTensors#
    -- @maxPerStageDescriptorSetStorageTensors@ is the maximum number of
    -- tensors that /can/ be accessible to a single shader stage in a pipeline
    -- layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM' count
    -- against this limit. A descriptor is accessible to a pipeline shader
    -- stage when the @stageFlags@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' structure has
    -- the bit for that shader stage set.
    maxPerStageDescriptorSetStorageTensors :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindStorageTensors#
    -- @maxDescriptorSetUpdateAfterBindStorageTensors@ is similar to
    -- @maxDescriptorSetStorageTensors@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageTensors :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindStorageTensors#
    -- @maxPerStageDescriptorUpdateAfterBindStorageTensors@ is similar to
    -- @maxPerStageDescriptorSetStorageTensors@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindStorageTensors :: Word32
  , -- | #limits-shaderStorageTensorArrayNonUniformIndexingNative#
    -- @shaderStorageTensorArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether storage tensor descriptors natively support
    -- nonuniform indexing. If this is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of storage buffers may execute multiple times in order
    -- to access all the descriptors.
    shaderStorageTensorArrayNonUniformIndexingNative :: Bool
  , -- | #limits-shaderTensorSupportedStages# @shaderTensorSupportedStages@ is a
    -- bitfield of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' describing
    -- the shader stages that /can/ access tensor resources.
    -- @shaderTensorSupportedStages@ will have the
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' bit
    -- set if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    shaderTensorSupportedStages :: ShaderStageFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTensorPropertiesARM)
#endif
deriving instance Show PhysicalDeviceTensorPropertiesARM

instance ToCStruct PhysicalDeviceTensorPropertiesARM where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTensorPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxTensorDimensionCount)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (maxTensorElements)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (maxPerDimensionTensorElements)
    poke ((p `plusPtr` 40 :: Ptr Int64)) (maxTensorStride)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (maxTensorSize)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxTensorShaderAccessArrayLength)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (maxTensorShaderAccessSize)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (maxDescriptorSetStorageTensors)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (maxPerStageDescriptorSetStorageTensors)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageTensors)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindStorageTensors)
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (shaderStorageTensorArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 84 :: Ptr ShaderStageFlags)) (shaderTensorSupportedStages)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Int64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 84 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceTensorPropertiesARM where
  peekCStruct p = do
    maxTensorDimensionCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxTensorElements <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    maxPerDimensionTensorElements <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    maxTensorStride <- peek @Int64 ((p `plusPtr` 40 :: Ptr Int64))
    maxTensorSize <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    maxTensorShaderAccessArrayLength <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    maxTensorShaderAccessSize <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxDescriptorSetStorageTensors <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    maxPerStageDescriptorSetStorageTensors <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindStorageTensors <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindStorageTensors <- peek @Word32 ((p `plusPtr` 76 :: Ptr Word32))
    shaderStorageTensorArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    shaderTensorSupportedStages <- peek @ShaderStageFlags ((p `plusPtr` 84 :: Ptr ShaderStageFlags))
    pure $ PhysicalDeviceTensorPropertiesARM
             maxTensorDimensionCount
             maxTensorElements
             maxPerDimensionTensorElements
             maxTensorStride
             maxTensorSize
             maxTensorShaderAccessArrayLength
             maxTensorShaderAccessSize
             maxDescriptorSetStorageTensors
             maxPerStageDescriptorSetStorageTensors
             maxDescriptorSetUpdateAfterBindStorageTensors
             maxPerStageDescriptorUpdateAfterBindStorageTensors
             (bool32ToBool shaderStorageTensorArrayNonUniformIndexingNative)
             shaderTensorSupportedStages

instance Storable PhysicalDeviceTensorPropertiesARM where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTensorPropertiesARM where
  zero = PhysicalDeviceTensorPropertiesARM
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


-- | VkTensorMemoryBarrierARM - Structure specifying a tensor memory barrier
--
-- = Description
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @srcStageMask@ and @srcAccessMask@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @dstStageMask@ and @dstAccessMask@.
--
-- Both
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
-- are limited to only memory accesses to @tensor@.
--
-- If @tensor@ was created with
-- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
-- @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, this memory
-- barrier defines a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>.
-- When executed on a queue in the family identified by
-- @srcQueueFamilyIndex@, this barrier defines a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified tensor, and the second synchronization and access
-- scopes do not synchronize operations on that queue. When executed on a
-- queue in the family identified by @dstQueueFamilyIndex@, this barrier
-- defines a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified tensor, and the first synchronization and access
-- scopes do not synchronize operations on that queue.
--
-- A
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- is also defined if the values are not equal, and either is one of the
-- special queue family values reserved for external memory ownership
-- transfers, as described in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers>.
-- A
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- is defined when @dstQueueFamilyIndex@ is one of those values, and a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- is defined when @srcQueueFamilyIndex@ is one of those values.
--
-- == Valid Usage
--
-- -   #VUID-VkTensorMemoryBarrierARM-tensor-09755# If @tensor@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ both be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   #VUID-VkTensorMemoryBarrierARM-tensor-09756# If @tensor@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ both be
--     either 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', or a valid
--     queue family (see
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#devsandqueues-queueprops>)
--
-- -   #VUID-VkTensorMemoryBarrierARM-tensor-09757# If @tensor@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', at least one of
--     them /must/ be the same as the family of the queue that will execute
--     this barrier
--
-- -   #VUID-VkTensorMemoryBarrierARM-tensor-09758# If @tensor@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkTensorMemoryBarrierARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_MEMORY_BARRIER_ARM'
--
-- -   #VUID-VkTensorMemoryBarrierARM-srcStageMask-parameter#
--     @srcStageMask@ /must/ be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-VkTensorMemoryBarrierARM-srcAccessMask-parameter#
--     @srcAccessMask@ /must/ be a valid combination of
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2' values
--
-- -   #VUID-VkTensorMemoryBarrierARM-dstStageMask-parameter#
--     @dstStageMask@ /must/ be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-VkTensorMemoryBarrierARM-dstAccessMask-parameter#
--     @dstAccessMask@ /must/ be a valid combination of
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2' values
--
-- -   #VUID-VkTensorMemoryBarrierARM-tensor-parameter# @tensor@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.TensorARM' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorARM', 'TensorDependencyInfoARM'
data TensorMemoryBarrierARM = TensorMemoryBarrierARM
  { -- | @srcStageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages to be included in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes first synchronization scope>.
    srcStageMask :: PipelineStageFlags2
  , -- | @srcAccessMask@ is a 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2'
    -- mask of access flags to be included in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes first access scope>.
    srcAccessMask :: AccessFlags2
  , -- | @dstStageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages to be included in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes second synchronization scope>.
    dstStageMask :: PipelineStageFlags2
  , -- | @dstAccessMask@ is a 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2'
    -- mask of access flags to be included in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes second access scope>.
    dstAccessMask :: AccessFlags2
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | @tensor@ is a handle to the tensor whose backing memory is affected by
    -- the barrier.
    tensor :: TensorARM
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorMemoryBarrierARM)
#endif
deriving instance Show TensorMemoryBarrierARM

instance ToCStruct TensorMemoryBarrierARM where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorMemoryBarrierARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_MEMORY_BARRIER_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2)) (srcStageMask)
    poke ((p `plusPtr` 24 :: Ptr AccessFlags2)) (srcAccessMask)
    poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2)) (dstStageMask)
    poke ((p `plusPtr` 40 :: Ptr AccessFlags2)) (dstAccessMask)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (srcQueueFamilyIndex)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (dstQueueFamilyIndex)
    poke ((p `plusPtr` 56 :: Ptr TensorARM)) (tensor)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_MEMORY_BARRIER_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr TensorARM)) (zero)
    f

instance FromCStruct TensorMemoryBarrierARM where
  peekCStruct p = do
    srcStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 16 :: Ptr PipelineStageFlags2))
    srcAccessMask <- peek @AccessFlags2 ((p `plusPtr` 24 :: Ptr AccessFlags2))
    dstStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 32 :: Ptr PipelineStageFlags2))
    dstAccessMask <- peek @AccessFlags2 ((p `plusPtr` 40 :: Ptr AccessFlags2))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    tensor <- peek @TensorARM ((p `plusPtr` 56 :: Ptr TensorARM))
    pure $ TensorMemoryBarrierARM
             srcStageMask
             srcAccessMask
             dstStageMask
             dstAccessMask
             srcQueueFamilyIndex
             dstQueueFamilyIndex
             tensor

instance Storable TensorMemoryBarrierARM where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TensorMemoryBarrierARM where
  zero = TensorMemoryBarrierARM
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkTensorDependencyInfoARM - Structure specifying tensor dependency
-- information for a synchronization command
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'TensorMemoryBarrierARM'
data TensorDependencyInfoARM = TensorDependencyInfoARM
  { -- | @tensorMemoryBarrierCount@ is the length of the @pTensorMemoryBarriers@
    -- array.
    tensorMemoryBarrierCount :: Word32
  , -- | @pTensorMemoryBarriers@ is a pointer to an array of
    -- 'TensorMemoryBarrierARM' structures defining memory dependencies between
    -- tensors.
    --
    -- #VUID-VkTensorDependencyInfoARM-pTensorMemoryBarriers-parameter#
    -- @pTensorMemoryBarriers@ /must/ be a valid pointer to a valid
    -- 'TensorMemoryBarrierARM' structure
    tensorMemoryBarriers :: TensorMemoryBarrierARM
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorDependencyInfoARM)
#endif
deriving instance Show TensorDependencyInfoARM

instance ToCStruct TensorDependencyInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorDependencyInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_DEPENDENCY_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (tensorMemoryBarrierCount)
    pTensorMemoryBarriers'' <- ContT $ withCStruct (tensorMemoryBarriers)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr TensorMemoryBarrierARM))) pTensorMemoryBarriers''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_DEPENDENCY_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    pTensorMemoryBarriers'' <- ContT $ withCStruct (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr TensorMemoryBarrierARM))) pTensorMemoryBarriers''
    lift $ f

instance FromCStruct TensorDependencyInfoARM where
  peekCStruct p = do
    tensorMemoryBarrierCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pTensorMemoryBarriers <- peekCStruct @TensorMemoryBarrierARM =<< peek ((p `plusPtr` 24 :: Ptr (Ptr TensorMemoryBarrierARM)))
    pure $ TensorDependencyInfoARM
             tensorMemoryBarrierCount pTensorMemoryBarriers

instance Zero TensorDependencyInfoARM where
  zero = TensorDependencyInfoARM
           zero
           zero


-- | VkPhysicalDeviceTensorFeaturesARM - Structure describing tensor features
-- that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceTensorFeaturesARM' structure describe
-- the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceTensorFeaturesARM' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceTensorFeaturesARM', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTensorFeaturesARM = PhysicalDeviceTensorFeaturesARM
  { -- | #features-tensorNonPacked# @tensorNonPacked@ indicates whether the
    -- implementation supports the creation of tensors that are not packed
    -- tensors.
    tensorNonPacked :: Bool
  , -- | #features-shaderTensorAccess# @shaderTensorAccess@ indicates whether
    -- shader modules /can/ declare the @TensorsARM@ capability.
    shaderTensorAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceTensorFeaturesARM" "shaderStorageTensorArrayDynamicIndexing"
    shaderStorageTensorArrayDynamicIndexing :: Bool
  , -- | #features-shaderStorageTensorArrayNonUniformIndexing#
    -- @shaderStorageTensorArrayNonUniformIndexing@ indicates whether arrays of
    -- storage tensors /can/ be indexed by non-uniform integer expressions in
    -- shader code. If this feature is not enabled, resources with a descriptor
    -- type of 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
    -- /must/ not be indexed by non-uniform integer expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @StorageTensorArrayNonUniformIndexingARM@ capability.
    shaderStorageTensorArrayNonUniformIndexing :: Bool
  , -- | #features-descriptorBindingStorageTensorUpdateAfterBind#
    -- @descriptorBindingStorageTensorUpdateAfterBind@ indicates whether the
    -- implementation supports updating storage tensor descriptors after a set
    -- is bound. If this feature is not enabled,
    -- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'.
    descriptorBindingStorageTensorUpdateAfterBind :: Bool
  , -- | #features-tensors# @tensors@ indicates whether the implementation
    -- supports tensor resources.
    tensors :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTensorFeaturesARM)
#endif
deriving instance Show PhysicalDeviceTensorFeaturesARM

instance ToCStruct PhysicalDeviceTensorFeaturesARM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTensorFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (tensorNonPacked))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderTensorAccess))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderStorageTensorArrayDynamicIndexing))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderStorageTensorArrayNonUniformIndexing))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (descriptorBindingStorageTensorUpdateAfterBind))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (tensors))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTensorFeaturesARM where
  peekCStruct p = do
    tensorNonPacked <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderTensorAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderStorageTensorArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderStorageTensorArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    descriptorBindingStorageTensorUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    tensors <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ PhysicalDeviceTensorFeaturesARM
             (bool32ToBool tensorNonPacked)
             (bool32ToBool shaderTensorAccess)
             (bool32ToBool shaderStorageTensorArrayDynamicIndexing)
             (bool32ToBool shaderStorageTensorArrayNonUniformIndexing)
             (bool32ToBool descriptorBindingStorageTensorUpdateAfterBind)
             (bool32ToBool tensors)

instance Storable PhysicalDeviceTensorFeaturesARM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTensorFeaturesARM where
  zero = PhysicalDeviceTensorFeaturesARM
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDeviceTensorMemoryRequirementsARM - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'TensorCreateInfoARM', 'getDeviceTensorMemoryRequirementsARM'
data DeviceTensorMemoryRequirementsARM = DeviceTensorMemoryRequirementsARM
  { -- | @pCreateInfo@ is a pointer to a 'TensorCreateInfoARM' structure
    -- containing parameters affecting the creation of the tensor to query.
    --
    -- #VUID-VkDeviceTensorMemoryRequirementsARM-pCreateInfo-parameter#
    -- @pCreateInfo@ /must/ be a valid pointer to a valid 'TensorCreateInfoARM'
    -- structure
    createInfo :: SomeStruct TensorCreateInfoARM }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceTensorMemoryRequirementsARM)
#endif
deriving instance Show DeviceTensorMemoryRequirementsARM

instance ToCStruct DeviceTensorMemoryRequirementsARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceTensorMemoryRequirementsARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_TENSOR_MEMORY_REQUIREMENTS_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (TensorCreateInfoARM '[])) $ \cont -> withSomeCStruct @TensorCreateInfoARM (createInfo) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (TensorCreateInfoARM _)))) pCreateInfo''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_TENSOR_MEMORY_REQUIREMENTS_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (TensorCreateInfoARM '[])) $ \cont -> withSomeCStruct @TensorCreateInfoARM ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (TensorCreateInfoARM _)))) pCreateInfo''
    lift $ f

instance FromCStruct DeviceTensorMemoryRequirementsARM where
  peekCStruct p = do
    pCreateInfo <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (TensorCreateInfoARM _))))
    pure $ DeviceTensorMemoryRequirementsARM
             pCreateInfo

instance Zero DeviceTensorMemoryRequirementsARM where
  zero = DeviceTensorMemoryRequirementsARM
           (SomeStruct zero)


-- | VkCopyTensorInfoARM - Structure specifying an tensor copy operation
--
-- = Description
--
-- Each region in @pRegions@ describes a region to be copied from the
-- source tensor to a corresponding region of the destination tensor.
-- @srcTensor@ and @dstTensor@ /can/ be the same tensor or alias the same
-- memory.
--
-- The formats of @srcTensor@ and @dstTensor@ /must/ be compatible. Formats
-- are compatible if they share the same class, as shown in the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-compatibility Compatible Formats>
-- table.
--
-- 'cmdCopyTensorARM' allows copying between /size-compatible/ internal
-- formats.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyTensorInfoARM-dimensionCount-09684# @srcTensor@ and
--     @dstTensor@ /must/ have been created with equal values for
--     'TensorDescriptionARM'::@dimensionCount@
--
-- -   #VUID-VkCopyTensorInfoARM-pDimensions-09685# For each of the
--     elements of 'TensorDescriptionARM'::@pDimensions@, @srcTensor@ and
--     @dstTensor@ /must/ be the same size
--
-- -   #VUID-VkCopyTensorInfoARM-regionCount-09686# @regionCount@ must be
--     equal to 1
--
-- -   #VUID-VkCopyTensorInfoARM-pRegions-09687# Each element of @pRegions@
--     /must/ be a 'TensorCopyARM' structure whose @pSrcOffset@ is @NULL@
--     or has all its elements equal to @0@
--
-- -   #VUID-VkCopyTensorInfoARM-pRegions-09688# Each element of @pRegions@
--     /must/ be a 'TensorCopyARM' structure whose @pDstOffset@ is @NULL@
--     or has all its elements equal to @0@
--
-- -   #VUID-VkCopyTensorInfoARM-pRegions-09689# Each element of @pRegions@
--     /must/ be a 'TensorCopyARM' structure whose @pExtent@ is @NULL@ or
--     equal to the 'TensorDescriptionARM'::@pDimensions@ array specified
--     when @srcTensor@ and @dstTensor@ were created
--
-- -   #VUID-VkCopyTensorInfoARM-pRegions-09954# Each element of @pRegions@
--     /must/ be a 'TensorCopyARM' structure whose @dimensionCount@, if it
--     is not equal to 0, is equal to the largest of the
--     'TensorDescriptionARM'::@dimensionCount@ of @srcTensor@ or
--     @dstTensor@
--
-- -   #VUID-VkCopyTensorInfoARM-srcTensor-09690# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-tensor-view-format-features format features>
--     of @srcTensor@ /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkCopyTensorInfoARM-srcTensor-09691# @srcTensor@ /must/ have
--     been created with the 'TENSOR_USAGE_TRANSFER_SRC_BIT_ARM' usage flag
--     set
--
-- -   #VUID-VkCopyTensorInfoARM-dstTensor-09692# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-tensor-view-format-features format features>
--     of @dstTensor@ /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_TRANSFER_DST_BIT'
--
-- -   #VUID-VkCopyTensorInfoARM-dstTensor-09693# @dstTensor@ /must/ have
--     been created with the 'TENSOR_USAGE_TRANSFER_DST_BIT_ARM' usage flag
--     set
--
-- -   #VUID-VkCopyTensorInfoARM-srcTensor-09694# If @srcTensor@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyTensorInfoARM-dstTensor-09695# If @dstTensor@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyTensorInfoARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_TENSOR_INFO_ARM'
--
-- -   #VUID-VkCopyTensorInfoARM-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyTensorInfoARM-srcTensor-parameter# @srcTensor@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.TensorARM' handle
--
-- -   #VUID-VkCopyTensorInfoARM-dstTensor-parameter# @dstTensor@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.TensorARM' handle
--
-- -   #VUID-VkCopyTensorInfoARM-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'TensorCopyARM'
--     structures
--
-- -   #VUID-VkCopyTensorInfoARM-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkCopyTensorInfoARM-commonparent# Both of @dstTensor@, and
--     @srcTensor@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorARM', 'TensorCopyARM',
-- 'cmdCopyTensorARM'
data CopyTensorInfoARM = CopyTensorInfoARM
  { -- | @srcTensor@ is the source tensor.
    srcTensor :: TensorARM
  , -- | @dstTensor@ is the destination tensor.
    dstTensor :: TensorARM
  , -- | @pRegions@ is a pointer to an array of 'TensorCopyARM' structures
    -- specifying the regions to copy.
    regions :: Vector TensorCopyARM
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyTensorInfoARM)
#endif
deriving instance Show CopyTensorInfoARM

instance ToCStruct CopyTensorInfoARM where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyTensorInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_TENSOR_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr TensorARM)) (srcTensor)
    lift $ poke ((p `plusPtr` 24 :: Ptr TensorARM)) (dstTensor)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @TensorCopyARM ((Data.Vector.length (regions)) * 48)
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRegions' `plusPtr` (48 * (i)) :: Ptr TensorCopyARM) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr TensorCopyARM))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_TENSOR_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (zero)
    poke ((p `plusPtr` 24 :: Ptr TensorARM)) (zero)
    f

instance FromCStruct CopyTensorInfoARM where
  peekCStruct p = do
    srcTensor <- peek @TensorARM ((p `plusPtr` 16 :: Ptr TensorARM))
    dstTensor <- peek @TensorARM ((p `plusPtr` 24 :: Ptr TensorARM))
    regionCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pRegions <- peek @(Ptr TensorCopyARM) ((p `plusPtr` 40 :: Ptr (Ptr TensorCopyARM)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @TensorCopyARM ((pRegions `advancePtrBytes` (48 * (i)) :: Ptr TensorCopyARM)))
    pure $ CopyTensorInfoARM
             srcTensor dstTensor pRegions'

instance Zero CopyTensorInfoARM where
  zero = CopyTensorInfoARM
           zero
           zero
           mempty


-- | VkTensorCopyARM - Structure specifying an tensor copy region
--
-- == Valid Usage
--
-- -   #VUID-VkTensorCopyARM-dimensionCount-09955# @dimensionCount@ /must/
--     be greater than 0 if @pSrcOffset@, @pDstOffset@, or @pExtent@ is not
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkTensorCopyARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_COPY_ARM'
--
-- -   #VUID-VkTensorCopyARM-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkTensorCopyARM-pSrcOffset-parameter# If @dimensionCount@ is
--     not @0@, and @pSrcOffset@ is not @NULL@, @pSrcOffset@ /must/ be a
--     valid pointer to an array of @dimensionCount@ @uint64_t@ values
--
-- -   #VUID-VkTensorCopyARM-pDstOffset-parameter# If @dimensionCount@ is
--     not @0@, and @pDstOffset@ is not @NULL@, @pDstOffset@ /must/ be a
--     valid pointer to an array of @dimensionCount@ @uint64_t@ values
--
-- -   #VUID-VkTensorCopyARM-pExtent-parameter# If @dimensionCount@ is not
--     @0@, and @pExtent@ is not @NULL@, @pExtent@ /must/ be a valid
--     pointer to an array of @dimensionCount@ @uint64_t@ values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'CopyTensorInfoARM', 'Vulkan.Core10.Enums.StructureType.StructureType'
data TensorCopyARM = TensorCopyARM
  { -- | @dimensionCount@ is the number of elements in the @pSrcOffset@,
    -- @pDstOffset@ and @pExtent@ arrays.
    dimensionCount :: Word32
  , -- | @pSrcOffset@ is @NULL@ or an array of size @dimensionCount@ providing an
    -- offset into the source tensor. When @pSrcOffset@ is @NULL@, the offset
    -- into the source tensor is @0@ in all dimensions.
    srcOffset :: Vector Word64
  , -- | @pDstOffset@ is @NULL@ or an array of size @dimensionCount@ providing an
    -- offset into the destination tensor. When @pDstOffset@ is @NULL@, the
    -- offset into the destination tensor is @0@ in all dimensions.
    dstOffset :: Vector Word64
  , -- | @pExtent@ is @NULL@ or an array of size @dimensionCount@ providing the
    -- number of elements to copy in each dimension. When @pExtent@ is @NULL@,
    -- the number of elements to copy is taken as the total number of elements
    -- in each dimension of the source tensor.
    extent :: Vector Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorCopyARM)
#endif
deriving instance Show TensorCopyARM

instance ToCStruct TensorCopyARM where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorCopyARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_COPY_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pSrcOffsetLength = Data.Vector.length $ (srcOffset)
    lift $ unless (fromIntegral pSrcOffsetLength == (dimensionCount) || pSrcOffsetLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pSrcOffset must be empty or have 'dimensionCount' elements" Nothing Nothing
    let pDstOffsetLength = Data.Vector.length $ (dstOffset)
    lift $ unless (fromIntegral pDstOffsetLength == (dimensionCount) || pDstOffsetLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pDstOffset must be empty or have 'dimensionCount' elements" Nothing Nothing
    let pExtentLength = Data.Vector.length $ (extent)
    lift $ unless (fromIntegral pExtentLength == (dimensionCount) || pExtentLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pExtent must be empty or have 'dimensionCount' elements" Nothing Nothing
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((dimensionCount))
    pSrcOffset'' <- if Data.Vector.null (srcOffset)
      then pure nullPtr
      else do
        pPSrcOffset <- ContT $ allocaBytes @Word64 (((Data.Vector.length (srcOffset))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPSrcOffset `plusPtr` (8 * (i)) :: Ptr Word64) (e)) ((srcOffset))
        pure $ pPSrcOffset
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word64))) pSrcOffset''
    pDstOffset'' <- if Data.Vector.null (dstOffset)
      then pure nullPtr
      else do
        pPDstOffset <- ContT $ allocaBytes @Word64 (((Data.Vector.length (dstOffset))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPDstOffset `plusPtr` (8 * (i)) :: Ptr Word64) (e)) ((dstOffset))
        pure $ pPDstOffset
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word64))) pDstOffset''
    pExtent'' <- if Data.Vector.null (extent)
      then pure nullPtr
      else do
        pPExtent <- ContT $ allocaBytes @Word64 (((Data.Vector.length (extent))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPExtent `plusPtr` (8 * (i)) :: Ptr Word64) (e)) ((extent))
        pure $ pPExtent
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word64))) pExtent''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_COPY_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct TensorCopyARM where
  peekCStruct p = do
    dimensionCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pSrcOffset <- peek @(Ptr Word64) ((p `plusPtr` 24 :: Ptr (Ptr Word64)))
    let pSrcOffsetLength = if pSrcOffset == nullPtr then 0 else (fromIntegral dimensionCount)
    pSrcOffset' <- generateM pSrcOffsetLength (\i -> peek @Word64 ((pSrcOffset `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pDstOffset <- peek @(Ptr Word64) ((p `plusPtr` 32 :: Ptr (Ptr Word64)))
    let pDstOffsetLength = if pDstOffset == nullPtr then 0 else (fromIntegral dimensionCount)
    pDstOffset' <- generateM pDstOffsetLength (\i -> peek @Word64 ((pDstOffset `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pExtent <- peek @(Ptr Word64) ((p `plusPtr` 40 :: Ptr (Ptr Word64)))
    let pExtentLength = if pExtent == nullPtr then 0 else (fromIntegral dimensionCount)
    pExtent' <- generateM pExtentLength (\i -> peek @Word64 ((pExtent `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ TensorCopyARM
             dimensionCount pSrcOffset' pDstOffset' pExtent'

instance Zero TensorCopyARM where
  zero = TensorCopyARM
           zero
           mempty
           mempty
           mempty


-- | VkMemoryDedicatedAllocateInfoTensorARM - Specify a dedicated memory
-- allocation tensor resource
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryDedicatedAllocateInfoTensorARM-allocationSize-09710#
--     'Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@ /must/
--     equal the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ of the
--     tensor
--
-- -   #VUID-VkMemoryDedicatedAllocateInfoTensorARM-tensor-09859# If
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' defines a memory import
--     operation with handle type
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT',
--     the memory being imported /must/ also be a dedicated tensor
--     allocation and @tensor@ /must/ be identical to the tensor associated
--     with the imported memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryDedicatedAllocateInfoTensorARM-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_TENSOR_ARM'
--
-- -   #VUID-VkMemoryDedicatedAllocateInfoTensorARM-tensor-parameter#
--     @tensor@ /must/ be a valid 'Vulkan.Extensions.Handles.TensorARM'
--     handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorARM'
data MemoryDedicatedAllocateInfoTensorARM = MemoryDedicatedAllocateInfoTensorARM
  { -- | @tensor@ is a handle of a tensor which this memory will be bound to.
    tensor :: TensorARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryDedicatedAllocateInfoTensorARM)
#endif
deriving instance Show MemoryDedicatedAllocateInfoTensorARM

instance ToCStruct MemoryDedicatedAllocateInfoTensorARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryDedicatedAllocateInfoTensorARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_TENSOR_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (tensor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_TENSOR_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (zero)
    f

instance FromCStruct MemoryDedicatedAllocateInfoTensorARM where
  peekCStruct p = do
    tensor <- peek @TensorARM ((p `plusPtr` 16 :: Ptr TensorARM))
    pure $ MemoryDedicatedAllocateInfoTensorARM
             tensor

instance Storable MemoryDedicatedAllocateInfoTensorARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryDedicatedAllocateInfoTensorARM where
  zero = MemoryDedicatedAllocateInfoTensorARM
           zero


-- | VkPhysicalDeviceDescriptorBufferTensorPropertiesARM - Structure
-- describing descriptor buffer tensor properties supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorBufferTensorPropertiesARM' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorBufferTensorPropertiesARM = PhysicalDeviceDescriptorBufferTensorPropertiesARM
  { -- | @tensorCaptureReplayDescriptorDataSize@ indicates the maximum size in
    -- bytes of the opaque data used for capture and replay with tensors.
    tensorCaptureReplayDescriptorDataSize :: Word64
  , -- | @tensorViewCaptureReplayDescriptorDataSize@ indicates the maximum size
    -- in bytes of the opaque data used for capture and replay with tensor
    -- views.
    tensorViewCaptureReplayDescriptorDataSize :: Word64
  , -- | @tensorDescriptorSize@ indicates the size in bytes of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
    -- descriptor.
    tensorDescriptorSize :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorBufferTensorPropertiesARM)
#endif
deriving instance Show PhysicalDeviceDescriptorBufferTensorPropertiesARM

instance ToCStruct PhysicalDeviceDescriptorBufferTensorPropertiesARM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorBufferTensorPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (tensorCaptureReplayDescriptorDataSize))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (tensorViewCaptureReplayDescriptorDataSize))
    poke ((p `plusPtr` 32 :: Ptr CSize)) (CSize (tensorDescriptorSize))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 32 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorBufferTensorPropertiesARM where
  peekCStruct p = do
    tensorCaptureReplayDescriptorDataSize <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    tensorViewCaptureReplayDescriptorDataSize <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    tensorDescriptorSize <- peek @CSize ((p `plusPtr` 32 :: Ptr CSize))
    pure $ PhysicalDeviceDescriptorBufferTensorPropertiesARM
             (coerce @CSize @Word64 tensorCaptureReplayDescriptorDataSize)
             (coerce @CSize @Word64 tensorViewCaptureReplayDescriptorDataSize)
             (coerce @CSize @Word64 tensorDescriptorSize)

instance Storable PhysicalDeviceDescriptorBufferTensorPropertiesARM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorBufferTensorPropertiesARM where
  zero = PhysicalDeviceDescriptorBufferTensorPropertiesARM
           zero
           zero
           zero


-- | VkPhysicalDeviceDescriptorBufferTensorFeaturesARM - Structure describing
-- the descriptor buffer tensor features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorBufferTensorFeaturesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDescriptorBufferTensorFeaturesARM', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorBufferTensorFeaturesARM = PhysicalDeviceDescriptorBufferTensorFeaturesARM
  { -- | #features-descriptorBufferTensorDescriptors#
    -- @descriptorBufferTensorDescriptors@ indicates that the implementation
    -- supports putthing shader-accessible tensor descriptors directly in
    -- memory.
    descriptorBufferTensorDescriptors :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorBufferTensorFeaturesARM)
#endif
deriving instance Show PhysicalDeviceDescriptorBufferTensorFeaturesARM

instance ToCStruct PhysicalDeviceDescriptorBufferTensorFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorBufferTensorFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (descriptorBufferTensorDescriptors))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorBufferTensorFeaturesARM where
  peekCStruct p = do
    descriptorBufferTensorDescriptors <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDescriptorBufferTensorFeaturesARM
             (bool32ToBool descriptorBufferTensorDescriptors)

instance Storable PhysicalDeviceDescriptorBufferTensorFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorBufferTensorFeaturesARM where
  zero = PhysicalDeviceDescriptorBufferTensorFeaturesARM
           zero


-- | VkTensorCaptureDescriptorDataInfoARM - Structure specifying a tensor for
-- descriptor capture
--
-- == Valid Usage
--
-- -   #VUID-VkTensorCaptureDescriptorDataInfoARM-tensor-09705# If @tensor@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' then @tensor@ /must/
--     have been created with
--     'TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM' set in
--     'TensorCreateInfoARM'::@flags@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkTensorCaptureDescriptorDataInfoARM-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_CAPTURE_DESCRIPTOR_DATA_INFO_ARM'
--
-- -   #VUID-VkTensorCaptureDescriptorDataInfoARM-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkTensorCaptureDescriptorDataInfoARM-tensor-parameter#
--     @tensor@ /must/ be a valid 'Vulkan.Extensions.Handles.TensorARM'
--     handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorARM',
-- 'getTensorOpaqueCaptureDescriptorDataARM'
data TensorCaptureDescriptorDataInfoARM = TensorCaptureDescriptorDataInfoARM
  { -- | @tensor@ is the 'Vulkan.Extensions.Handles.TensorARM' handle of the
    -- tensor to get opaque capture data for.
    tensor :: TensorARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorCaptureDescriptorDataInfoARM)
#endif
deriving instance Show TensorCaptureDescriptorDataInfoARM

instance ToCStruct TensorCaptureDescriptorDataInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorCaptureDescriptorDataInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_CAPTURE_DESCRIPTOR_DATA_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (tensor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_CAPTURE_DESCRIPTOR_DATA_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorARM)) (zero)
    f

instance FromCStruct TensorCaptureDescriptorDataInfoARM where
  peekCStruct p = do
    tensor <- peek @TensorARM ((p `plusPtr` 16 :: Ptr TensorARM))
    pure $ TensorCaptureDescriptorDataInfoARM
             tensor

instance Storable TensorCaptureDescriptorDataInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TensorCaptureDescriptorDataInfoARM where
  zero = TensorCaptureDescriptorDataInfoARM
           zero


-- | VkTensorViewCaptureDescriptorDataInfoARM - Structure specifying a tensor
-- view for descriptor capture
--
-- == Valid Usage
--
-- -   #VUID-VkTensorViewCaptureDescriptorDataInfoARM-tensorView-09709# If
--     @tensorView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' then
--     @tensorView@ /must/ have been created with
--     'TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM' set in
--     'TensorViewCreateInfoARM'::@flags@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkTensorViewCaptureDescriptorDataInfoARM-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_ARM'
--
-- -   #VUID-VkTensorViewCaptureDescriptorDataInfoARM-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkTensorViewCaptureDescriptorDataInfoARM-tensorView-parameter#
--     @tensorView@ /must/ be a valid
--     'Vulkan.Extensions.Handles.TensorViewARM' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorViewARM',
-- 'getTensorViewOpaqueCaptureDescriptorDataARM'
data TensorViewCaptureDescriptorDataInfoARM = TensorViewCaptureDescriptorDataInfoARM
  { -- | @tensorView@ is the 'Vulkan.Extensions.Handles.TensorViewARM' handle of
    -- the tensor view to get opaque capture data for.
    tensorView :: TensorViewARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TensorViewCaptureDescriptorDataInfoARM)
#endif
deriving instance Show TensorViewCaptureDescriptorDataInfoARM

instance ToCStruct TensorViewCaptureDescriptorDataInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TensorViewCaptureDescriptorDataInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorViewARM)) (tensorView)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TENSOR_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorViewARM)) (zero)
    f

instance FromCStruct TensorViewCaptureDescriptorDataInfoARM where
  peekCStruct p = do
    tensorView <- peek @TensorViewARM ((p `plusPtr` 16 :: Ptr TensorViewARM))
    pure $ TensorViewCaptureDescriptorDataInfoARM
             tensorView

instance Storable TensorViewCaptureDescriptorDataInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TensorViewCaptureDescriptorDataInfoARM where
  zero = TensorViewCaptureDescriptorDataInfoARM
           zero


-- | VkDescriptorGetTensorInfoARM - Structure specifying parameters to get
-- descriptor data for tensor views
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorGetTensorInfoARM-nullDescriptor-09899# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, @tensorView@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorGetTensorInfoARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_GET_TENSOR_INFO_ARM'
--
-- -   #VUID-VkDescriptorGetTensorInfoARM-tensorView-parameter# If
--     @tensorView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @tensorView@ /must/ be a valid
--     'Vulkan.Extensions.Handles.TensorViewARM' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorViewARM'
data DescriptorGetTensorInfoARM = DescriptorGetTensorInfoARM
  { -- | @tensorView@ is a 'Vulkan.Extensions.Handles.TensorViewARM' handle
    -- specifying the parameters of a
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
    -- descriptor.
    tensorView :: TensorViewARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorGetTensorInfoARM)
#endif
deriving instance Show DescriptorGetTensorInfoARM

instance ToCStruct DescriptorGetTensorInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorGetTensorInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_GET_TENSOR_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TensorViewARM)) (tensorView)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_GET_TENSOR_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DescriptorGetTensorInfoARM where
  peekCStruct p = do
    tensorView <- peek @TensorViewARM ((p `plusPtr` 16 :: Ptr TensorViewARM))
    pure $ DescriptorGetTensorInfoARM
             tensorView

instance Storable DescriptorGetTensorInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorGetTensorInfoARM where
  zero = DescriptorGetTensorInfoARM
           zero


-- | VkFrameBoundaryTensorsARM - Add tensor frame boundary information to
-- queue submissions
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.TensorARM'
data FrameBoundaryTensorsARM = FrameBoundaryTensorsARM
  { -- | @pTensors@ is a pointer to an array of
    -- 'Vulkan.Extensions.Handles.TensorARM' objects with tensorCount entries.
    --
    -- #VUID-VkFrameBoundaryTensorsARM-pTensors-parameter# @pTensors@ /must/ be
    -- a valid pointer to an array of @tensorCount@ valid
    -- 'Vulkan.Extensions.Handles.TensorARM' handles
    tensors :: Vector TensorARM }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FrameBoundaryTensorsARM)
#endif
deriving instance Show FrameBoundaryTensorsARM

instance ToCStruct FrameBoundaryTensorsARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FrameBoundaryTensorsARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAME_BOUNDARY_TENSORS_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (tensors)) :: Word32))
    pPTensors' <- ContT $ allocaBytes @TensorARM ((Data.Vector.length (tensors)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPTensors' `plusPtr` (8 * (i)) :: Ptr TensorARM) (e)) (tensors)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr TensorARM))) (pPTensors')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAME_BOUNDARY_TENSORS_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct FrameBoundaryTensorsARM where
  peekCStruct p = do
    tensorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pTensors <- peek @(Ptr TensorARM) ((p `plusPtr` 24 :: Ptr (Ptr TensorARM)))
    pTensors' <- generateM (fromIntegral tensorCount) (\i -> peek @TensorARM ((pTensors `advancePtrBytes` (8 * (i)) :: Ptr TensorARM)))
    pure $ FrameBoundaryTensorsARM
             pTensors'

instance Zero FrameBoundaryTensorsARM where
  zero = FrameBoundaryTensorsARM
           mempty


-- | VkPhysicalDeviceExternalTensorInfoARM - Structure specifying tensor
-- creation parameters.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'TensorCreateFlagsARM', 'TensorDescriptionARM',
-- 'getPhysicalDeviceExternalTensorPropertiesARM'
data PhysicalDeviceExternalTensorInfoARM = PhysicalDeviceExternalTensorInfoARM
  { -- | @flags@ is a bitmask of 'TensorCreateFlagBitsARM' describing additional
    -- parameters of the tensor, corresponding to
    -- 'TensorCreateInfoARM'::@flags@.
    --
    -- #VUID-VkPhysicalDeviceExternalTensorInfoARM-flags-parameter# @flags@
    -- /must/ be a valid combination of 'TensorCreateFlagBitsARM' values
    flags :: TensorCreateFlagsARM
  , -- | @pDescription@ is a 'TensorDescriptionARM' structure describing the
    -- tensor, corresponding to 'TensorCreateInfoARM'::@pDescription@.
    --
    -- #VUID-VkPhysicalDeviceExternalTensorInfoARM-pDescription-parameter#
    -- @pDescription@ /must/ be a valid pointer to a valid
    -- 'TensorDescriptionARM' structure
    description :: TensorDescriptionARM
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the external memory handle type for which capabilities
    -- will be returned.
    --
    -- #VUID-VkPhysicalDeviceExternalTensorInfoARM-handleType-parameter#
    -- @handleType@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalTensorInfoARM)
#endif
deriving instance Show PhysicalDeviceExternalTensorInfoARM

instance ToCStruct PhysicalDeviceExternalTensorInfoARM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalTensorInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_TENSOR_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr TensorCreateFlagsARM)) (flags)
    pDescription'' <- ContT $ withCStruct (description)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr TensorDescriptionARM))) pDescription''
    lift $ poke ((p `plusPtr` 32 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_TENSOR_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pDescription'' <- ContT $ withCStruct (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr TensorDescriptionARM))) pDescription''
    lift $ poke ((p `plusPtr` 32 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    lift $ f

instance FromCStruct PhysicalDeviceExternalTensorInfoARM where
  peekCStruct p = do
    flags <- peek @TensorCreateFlagsARM ((p `plusPtr` 16 :: Ptr TensorCreateFlagsARM))
    pDescription <- peekCStruct @TensorDescriptionARM =<< peek ((p `plusPtr` 24 :: Ptr (Ptr TensorDescriptionARM)))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 32 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ PhysicalDeviceExternalTensorInfoARM
             flags pDescription handleType

instance Zero PhysicalDeviceExternalTensorInfoARM where
  zero = PhysicalDeviceExternalTensorInfoARM
           zero
           zero
           zero


-- | VkExternalTensorPropertiesARM - Structure specifying supported external
-- handle capabilities for a tensor
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalMemoryProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalTensorPropertiesARM'
data ExternalTensorPropertiesARM = ExternalTensorPropertiesARM
  { -- | @externalMemoryProperties@ is a
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalMemoryProperties'
    -- structure specifying various capabilities of the external handle type
    -- when used with the specified tensor creation parameters.
    --
    -- #VUID-VkExternalTensorPropertiesARM-externalMemoryProperties-parameter#
    -- @externalMemoryProperties@ /must/ be a valid
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalMemoryProperties'
    -- structure
    externalMemoryProperties :: ExternalMemoryProperties }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalTensorPropertiesARM)
#endif
deriving instance Show ExternalTensorPropertiesARM

instance ToCStruct ExternalTensorPropertiesARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalTensorPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_TENSOR_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (externalMemoryProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_TENSOR_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (zero)
    f

instance FromCStruct ExternalTensorPropertiesARM where
  peekCStruct p = do
    externalMemoryProperties <- peekCStruct @ExternalMemoryProperties ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties))
    pure $ ExternalTensorPropertiesARM
             externalMemoryProperties

instance Storable ExternalTensorPropertiesARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalTensorPropertiesARM where
  zero = ExternalTensorPropertiesARM
           zero


-- | VkExternalMemoryTensorCreateInfoARM - Specify that a tensor may be
-- backed by external memory
--
-- = Members
--
-- A 'ExternalMemoryTensorCreateInfoARM' structure with a non-zero
-- @handleTypes@ field must be included in the creation parameters for a
-- tensor that will be bound to memory that is either exported or imported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalMemoryTensorCreateInfoARM = ExternalMemoryTensorCreateInfoARM
  { -- | @handleTypes@ is zero or a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- specifying one or more external memory handle types.
    --
    -- #VUID-VkExternalMemoryTensorCreateInfoARM-handleTypes-parameter#
    -- @handleTypes@ /must/ be a valid combination of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- values
    handleTypes :: ExternalMemoryHandleTypeFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalMemoryTensorCreateInfoARM)
#endif
deriving instance Show ExternalMemoryTensorCreateInfoARM

instance ToCStruct ExternalMemoryTensorCreateInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalMemoryTensorCreateInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_TENSOR_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_TENSOR_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExternalMemoryTensorCreateInfoARM where
  peekCStruct p = do
    handleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags))
    pure $ ExternalMemoryTensorCreateInfoARM
             handleTypes

instance Storable ExternalMemoryTensorCreateInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalMemoryTensorCreateInfoARM where
  zero = ExternalMemoryTensorCreateInfoARM
           zero


type TensorCreateFlagsARM = TensorCreateFlagBitsARM

-- | VkTensorCreateFlagBitsARM - Bitmask specifying additional parameters of
-- a tensor
--
-- = Description
--
-- -   'TENSOR_CREATE_MUTABLE_FORMAT_BIT_ARM' specifies that the tensor
--     /can/ be used to create a 'Vulkan.Extensions.Handles.TensorViewARM'
--     with a different format from the tensor.
--
-- -   'TENSOR_CREATE_PROTECTED_BIT_ARM' specifies that the tensor is a
--     protected tensor.
--
-- -   'TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM' specifies
--     that the tensor /can/ be used with descriptor buffers when capturing
--     and replaying (e.g. for trace capture and replay), see
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
--     for more detail.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'TensorCreateFlagsARM'
newtype TensorCreateFlagBitsARM = TensorCreateFlagBitsARM Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkTensorCreateFlagBitsARM" "VK_TENSOR_CREATE_MUTABLE_FORMAT_BIT_ARM"
pattern TENSOR_CREATE_MUTABLE_FORMAT_BIT_ARM = TensorCreateFlagBitsARM 0x0000000000000001

-- No documentation found for Nested "VkTensorCreateFlagBitsARM" "VK_TENSOR_CREATE_PROTECTED_BIT_ARM"
pattern TENSOR_CREATE_PROTECTED_BIT_ARM = TensorCreateFlagBitsARM 0x0000000000000002

-- No documentation found for Nested "VkTensorCreateFlagBitsARM" "VK_TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM"
pattern TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM = TensorCreateFlagBitsARM 0x0000000000000004

-- No documentation found for Nested "VkTensorCreateFlagBitsARM" "VK_TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM"
pattern TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM = TensorCreateFlagBitsARM 0x0000000000000008

conNameTensorCreateFlagBitsARM :: String
conNameTensorCreateFlagBitsARM = "TensorCreateFlagBitsARM"

enumPrefixTensorCreateFlagBitsARM :: String
enumPrefixTensorCreateFlagBitsARM = "TENSOR_CREATE_"

showTableTensorCreateFlagBitsARM :: [(TensorCreateFlagBitsARM, String)]
showTableTensorCreateFlagBitsARM =
  [
    ( TENSOR_CREATE_MUTABLE_FORMAT_BIT_ARM
    , "MUTABLE_FORMAT_BIT_ARM"
    )
  ,
    ( TENSOR_CREATE_PROTECTED_BIT_ARM
    , "PROTECTED_BIT_ARM"
    )
  ,
    ( TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM
    , "DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM"
    )
  ,
    ( TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM
    , "DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM"
    )
  ]

instance Show TensorCreateFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixTensorCreateFlagBitsARM
      showTableTensorCreateFlagBitsARM
      conNameTensorCreateFlagBitsARM
      (\(TensorCreateFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read TensorCreateFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixTensorCreateFlagBitsARM
      showTableTensorCreateFlagBitsARM
      conNameTensorCreateFlagBitsARM
      TensorCreateFlagBitsARM

type TensorUsageFlagsARM = TensorUsageFlagBitsARM

-- | VkTensorUsageFlagBitsARM - Bitmask specifying allowed usage of a tensor
--
-- = Description
--
-- -   'TENSOR_USAGE_SHADER_BIT_ARM' specifies that the tensor /can/ be
--     used to create a 'Vulkan.Extensions.Handles.TensorViewARM' suitable
--     for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     accessed by shader stages.
--
-- -   'TENSOR_USAGE_TRANSFER_SRC_BIT_ARM' specifies that the tensor /can/
--     be used as the source of a /transfer command/ (see the definition of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-pipeline-stages-transfer >).
--
-- -   'TENSOR_USAGE_TRANSFER_DST_BIT_ARM' specifies that the tensor /can/
--     be used as the destination of a transfer command.
--
-- -   'TENSOR_USAGE_IMAGE_ALIASING_BIT_ARM' specifies that the tensor
--     /can/ be bound to a range of memory aliased with an image created
--     with 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'. See
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-memory-aliasing>
--     for a complete set of rules for tensor\/image aliasing.
--
-- -   'TENSOR_USAGE_DATA_GRAPH_BIT_ARM' specifies that the tensor /can/ be
--     used to create a 'Vulkan.Extensions.Handles.TensorViewARM' suitable
--     for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     accessed by
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#graphs-pipelines data graph pipelines>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'TensorUsageFlagsARM'
newtype TensorUsageFlagBitsARM = TensorUsageFlagBitsARM Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkTensorUsageFlagBitsARM" "VK_TENSOR_USAGE_SHADER_BIT_ARM"
pattern TENSOR_USAGE_SHADER_BIT_ARM = TensorUsageFlagBitsARM 0x0000000000000002

-- No documentation found for Nested "VkTensorUsageFlagBitsARM" "VK_TENSOR_USAGE_TRANSFER_SRC_BIT_ARM"
pattern TENSOR_USAGE_TRANSFER_SRC_BIT_ARM = TensorUsageFlagBitsARM 0x0000000000000004

-- No documentation found for Nested "VkTensorUsageFlagBitsARM" "VK_TENSOR_USAGE_TRANSFER_DST_BIT_ARM"
pattern TENSOR_USAGE_TRANSFER_DST_BIT_ARM = TensorUsageFlagBitsARM 0x0000000000000008

-- No documentation found for Nested "VkTensorUsageFlagBitsARM" "VK_TENSOR_USAGE_IMAGE_ALIASING_BIT_ARM"
pattern TENSOR_USAGE_IMAGE_ALIASING_BIT_ARM = TensorUsageFlagBitsARM 0x0000000000000010

-- No documentation found for Nested "VkTensorUsageFlagBitsARM" "VK_TENSOR_USAGE_DATA_GRAPH_BIT_ARM"
pattern TENSOR_USAGE_DATA_GRAPH_BIT_ARM = TensorUsageFlagBitsARM 0x0000000000000020

conNameTensorUsageFlagBitsARM :: String
conNameTensorUsageFlagBitsARM = "TensorUsageFlagBitsARM"

enumPrefixTensorUsageFlagBitsARM :: String
enumPrefixTensorUsageFlagBitsARM = "TENSOR_USAGE_"

showTableTensorUsageFlagBitsARM :: [(TensorUsageFlagBitsARM, String)]
showTableTensorUsageFlagBitsARM =
  [
    ( TENSOR_USAGE_SHADER_BIT_ARM
    , "SHADER_BIT_ARM"
    )
  ,
    ( TENSOR_USAGE_TRANSFER_SRC_BIT_ARM
    , "TRANSFER_SRC_BIT_ARM"
    )
  ,
    ( TENSOR_USAGE_TRANSFER_DST_BIT_ARM
    , "TRANSFER_DST_BIT_ARM"
    )
  ,
    ( TENSOR_USAGE_IMAGE_ALIASING_BIT_ARM
    , "IMAGE_ALIASING_BIT_ARM"
    )
  ,
    ( TENSOR_USAGE_DATA_GRAPH_BIT_ARM
    , "DATA_GRAPH_BIT_ARM"
    )
  ]

instance Show TensorUsageFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixTensorUsageFlagBitsARM
      showTableTensorUsageFlagBitsARM
      conNameTensorUsageFlagBitsARM
      (\(TensorUsageFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read TensorUsageFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixTensorUsageFlagBitsARM
      showTableTensorUsageFlagBitsARM
      conNameTensorUsageFlagBitsARM
      TensorUsageFlagBitsARM

-- | VkTensorTilingARM - Specifies the tiling arrangement of data in an
-- tensor
--
-- = Description
--
-- -   'TENSOR_TILING_OPTIMAL_ARM' specifies optimal tiling (elements are
--     laid out in an implementation-dependent arrangement, for more
--     efficient memory access).
--
-- -   'TENSOR_TILING_LINEAR_ARM' specifies linear tiling (elements are
--     laid out linearly and the offset between each element is determined
--     by the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-tensor-description-strides strides>
--     of the tensor).
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'TensorDescriptionARM'
newtype TensorTilingARM = TensorTilingARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkTensorTilingARM" "VK_TENSOR_TILING_OPTIMAL_ARM"
pattern TENSOR_TILING_OPTIMAL_ARM = TensorTilingARM 0

-- No documentation found for Nested "VkTensorTilingARM" "VK_TENSOR_TILING_LINEAR_ARM"
pattern TENSOR_TILING_LINEAR_ARM = TensorTilingARM 1

{-# COMPLETE
  TENSOR_TILING_OPTIMAL_ARM
  , TENSOR_TILING_LINEAR_ARM ::
    TensorTilingARM
  #-}

conNameTensorTilingARM :: String
conNameTensorTilingARM = "TensorTilingARM"

enumPrefixTensorTilingARM :: String
enumPrefixTensorTilingARM = "TENSOR_TILING_"

showTableTensorTilingARM :: [(TensorTilingARM, String)]
showTableTensorTilingARM =
  [ (TENSOR_TILING_OPTIMAL_ARM, "OPTIMAL_ARM")
  , (TENSOR_TILING_LINEAR_ARM, "LINEAR_ARM")
  ]

instance Show TensorTilingARM where
  showsPrec =
    enumShowsPrec
      enumPrefixTensorTilingARM
      showTableTensorTilingARM
      conNameTensorTilingARM
      (\(TensorTilingARM x) -> x)
      (showsPrec 11)

instance Read TensorTilingARM where
  readPrec =
    enumReadPrec
      enumPrefixTensorTilingARM
      showTableTensorTilingARM
      conNameTensorTilingARM
      TensorTilingARM

type TensorViewCreateFlagsARM = TensorViewCreateFlagBitsARM

-- | VkTensorViewCreateFlagBitsARM - Bitmask specifying additional parameters
-- of an tensor view
--
-- = Description
--
-- -   'TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM'
--     specifies that the tensor view /can/ be used with descriptor buffers
--     when capturing and replaying (e.g. for trace capture and replay),
--     see
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
--     for more detail.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'TensorViewCreateFlagsARM'
newtype TensorViewCreateFlagBitsARM = TensorViewCreateFlagBitsARM Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkTensorViewCreateFlagBitsARM" "VK_TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM"
pattern TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM = TensorViewCreateFlagBitsARM 0x0000000000000001

conNameTensorViewCreateFlagBitsARM :: String
conNameTensorViewCreateFlagBitsARM = "TensorViewCreateFlagBitsARM"

enumPrefixTensorViewCreateFlagBitsARM :: String
enumPrefixTensorViewCreateFlagBitsARM = "TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM"

showTableTensorViewCreateFlagBitsARM :: [(TensorViewCreateFlagBitsARM, String)]
showTableTensorViewCreateFlagBitsARM =
  [
    ( TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM
    , ""
    )
  ]

instance Show TensorViewCreateFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixTensorViewCreateFlagBitsARM
      showTableTensorViewCreateFlagBitsARM
      conNameTensorViewCreateFlagBitsARM
      (\(TensorViewCreateFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read TensorViewCreateFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixTensorViewCreateFlagBitsARM
      showTableTensorViewCreateFlagBitsARM
      conNameTensorViewCreateFlagBitsARM
      TensorViewCreateFlagBitsARM

type ARM_TENSORS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_ARM_TENSORS_SPEC_VERSION"
pattern ARM_TENSORS_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_TENSORS_SPEC_VERSION = 2


type ARM_TENSORS_EXTENSION_NAME = "VK_ARM_tensors"

-- No documentation found for TopLevel "VK_ARM_TENSORS_EXTENSION_NAME"
pattern ARM_TENSORS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_TENSORS_EXTENSION_NAME = "VK_ARM_tensors"

