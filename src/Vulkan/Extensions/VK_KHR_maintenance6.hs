{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance6 - device extension
--
-- = VK_KHR_maintenance6
--
-- [__Name String__]
--     @VK_KHR_maintenance6@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     546
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_descriptor_buffer
--
--     -   Interacts with VK_KHR_push_descriptor
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
--
-- [__Contact__]
--
--     -   Jon Leech
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance6] @oddhack%0A*Here describe the issue or question you have about the VK_KHR_maintenance6 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance6.adoc VK_KHR_maintenance6>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-08-03
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_EXT_robustness2@
--
-- [__Contributors__]
--
--     -   Jon Leech, Khronos
--
--     -   Stu Smith, AMD
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Ralph Potter, Samsung
--
--     -   James Fitzpatrick, Imagination Technologies
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   'BindMemoryStatusKHR' may be included in the @pNext@ chain of
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
--     and
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
--     allowing applications to identify individual resources for which
--     memory binding failed during calls to
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindBufferMemory2'
--     and
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindImageMemory2'.
--
-- -   A new property @fragmentShadingRateClampCombinerInputs@ to indicate
--     if an implementation clamps the inputs to fragment shading rate
--     combiner operations.
--
-- -   'Vulkan.Core10.APIConstants.NULL_HANDLE' is allowed to be used when
--     binding an index buffer, instead of a valid
--     'Vulkan.Core10.Handles.Buffer' handle. When the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is enabled, every index fetched results in a value of zero.
--
-- -   A new property @maxCombinedImageSamplerDescriptorCount@ to indicate
--     the maximum number of descriptors needed for any of the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>
--     supported by the implementation.
--
-- -   A new property @blockTexelViewCompatibleMultipleLayers@ indicating
--     whether
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     is allowed to be used with @layerCount@ > 1
--
-- -   @pNext@ extensible *2 versions of all descriptor binding commands.
--
-- == New Commands
--
-- -   'cmdBindDescriptorSets2KHR'
--
-- -   'cmdPushConstants2KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   'cmdBindDescriptorBufferEmbeddedSamplers2EXT'
--
-- -   'cmdSetDescriptorBufferOffsets2EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   'cmdPushDescriptorSet2KHR'
--
-- -   'cmdPushDescriptorSetWithTemplate2KHR'
--
-- == New Structures
--
-- -   'BindDescriptorSetsInfoKHR'
--
-- -   'PushConstantsInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo':
--
--     -   'BindMemoryStatusKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance6FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance6PropertiesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   'BindDescriptorBufferEmbeddedSamplersInfoEXT'
--
-- -   'SetDescriptorBufferOffsetsInfoEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   'PushDescriptorSetInfoKHR'
--
-- -   'PushDescriptorSetWithTemplateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_6_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_6_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- KHR suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2023-08-01 (Jon Leech)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance6 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance6  ( cmdSetDescriptorBufferOffsets2EXT
                                              , cmdBindDescriptorBufferEmbeddedSamplers2EXT
                                              , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR
                                              , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR
                                              , pattern STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR
                                              , pattern STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR
                                              , pattern STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR
                                              , pattern STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR
                                              , pattern STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR
                                              , cmdBindDescriptorSets2KHR
                                              , cmdPushConstants2KHR
                                              , cmdPushDescriptorSet2KHR
                                              , cmdPushDescriptorSetWithTemplate2KHR
                                              , SetDescriptorBufferOffsetsInfoEXT(..)
                                              , BindDescriptorBufferEmbeddedSamplersInfoEXT(..)
                                              , PhysicalDeviceMaintenance6FeaturesKHR
                                              , PhysicalDeviceMaintenance6PropertiesKHR
                                              , BindMemoryStatusKHR
                                              , BindDescriptorSetsInfoKHR
                                              , PushConstantsInfoKHR
                                              , PushDescriptorSetInfoKHR
                                              , PushDescriptorSetWithTemplateInfoKHR
                                              , KHR_MAINTENANCE_6_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_6_SPEC_VERSION
                                              , KHR_MAINTENANCE_6_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_6_EXTENSION_NAME
                                              ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
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
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
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
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (cmdBindDescriptorSets2)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (cmdPushConstants2)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (cmdPushDescriptorSet2)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (cmdPushDescriptorSetWithTemplate2)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (BindDescriptorSetsInfo)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (BindMemoryStatus)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorBufferEmbeddedSamplers2EXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDescriptorBufferOffsets2EXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (PhysicalDeviceMaintenance6Features)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (PhysicalDeviceMaintenance6Properties)
import Vulkan.Core10.Handles (PipelineLayout)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PipelineLayoutCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (PushConstantsInfo)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (PushDescriptorSetInfo)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality' (PushDescriptorSetWithTemplateInfo)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_MEMORY_STATUS))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_CONSTANTS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDescriptorBufferOffsets2EXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct SetDescriptorBufferOffsetsInfoEXT) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct SetDescriptorBufferOffsetsInfoEXT) -> IO ()

-- | vkCmdSetDescriptorBufferOffsets2EXT - Setting descriptor buffer offsets
-- in a command buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-commandBuffer-11295# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-commandBuffer-11296# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-descriptorBuffer-09470#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-pSetDescriptorBufferOffsetsInfo-09471#
--     Each bit in @pSetDescriptorBufferOffsetsInfo->stageFlags@ /must/ be
--     a stage supported by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-pSetDescriptorBufferOffsetsInfo-parameter#
--     @pSetDescriptorBufferOffsetsInfo@ /must/ be a valid pointer to a
--     valid 'SetDescriptorBufferOffsetsInfoEXT' structure
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_DATA_GRAPH_BIT_ARM', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_DATA_GRAPH_BIT_ARM                                                                                           |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSetDescriptorBufferOffsets2EXT is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'SetDescriptorBufferOffsetsInfoEXT'
cmdSetDescriptorBufferOffsets2EXT :: forall a io
                                   . ( Extendss SetDescriptorBufferOffsetsInfoEXT a
                                     , PokeChain a
                                     , MonadIO io )
                                  => -- | @commandBuffer@ is the command buffer in which the descriptor buffer
                                     -- offsets will be set.
                                     CommandBuffer
                                  -> -- | @pSetDescriptorBufferOffsetsInfo@ is a pointer to a
                                     -- 'SetDescriptorBufferOffsetsInfoEXT' structure.
                                     (SetDescriptorBufferOffsetsInfoEXT a)
                                  -> io ()
cmdSetDescriptorBufferOffsets2EXT commandBuffer
                                    setDescriptorBufferOffsetsInfo = liftIO . evalContT $ do
  let vkCmdSetDescriptorBufferOffsets2EXTPtr = pVkCmdSetDescriptorBufferOffsets2EXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetDescriptorBufferOffsets2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDescriptorBufferOffsets2EXT is null" Nothing Nothing
  let vkCmdSetDescriptorBufferOffsets2EXT' = mkVkCmdSetDescriptorBufferOffsets2EXT vkCmdSetDescriptorBufferOffsets2EXTPtr
  pSetDescriptorBufferOffsetsInfo <- ContT $ withCStruct (setDescriptorBufferOffsetsInfo)
  lift $ traceAroundEvent "vkCmdSetDescriptorBufferOffsets2EXT" (vkCmdSetDescriptorBufferOffsets2EXT'
                                                                   (commandBufferHandle (commandBuffer))
                                                                   (forgetExtensions pSetDescriptorBufferOffsetsInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorBufferEmbeddedSamplers2EXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct BindDescriptorBufferEmbeddedSamplersInfoEXT) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct BindDescriptorBufferEmbeddedSamplersInfoEXT) -> IO ()

-- | vkCmdBindDescriptorBufferEmbeddedSamplers2EXT - Setting embedded
-- immutable samplers offsets in a command buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-commandBuffer-11295#
--     If @commandBuffer@ is a secondary command buffer, it /must/ have
--     begun with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-commandBuffer-11296#
--     If @commandBuffer@ is a secondary command buffer, it /must/ have
--     begun with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-descriptorBuffer-09472#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-pBindDescriptorBufferEmbeddedSamplersInfo-09473#
--     Each bit in @pBindDescriptorBufferEmbeddedSamplersInfo->stageFlags@
--     /must/ be a stage supported by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-pBindDescriptorBufferEmbeddedSamplersInfo-parameter#
--     @pBindDescriptorBufferEmbeddedSamplersInfo@ /must/ be a valid
--     pointer to a valid 'BindDescriptorBufferEmbeddedSamplersInfoEXT'
--     structure
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-videocoding#
--     This command /must/ only be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBindDescriptorBufferEmbeddedSamplers2EXT is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'BindDescriptorBufferEmbeddedSamplersInfoEXT',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdBindDescriptorBufferEmbeddedSamplers2EXT :: forall a io
                                             . ( Extendss BindDescriptorBufferEmbeddedSamplersInfoEXT a
                                               , PokeChain a
                                               , MonadIO io )
                                            => -- | @commandBuffer@ is the command buffer that the embedded immutable
                                               -- samplers will be bound to.
                                               CommandBuffer
                                            -> -- | @pBindDescriptorBufferEmbeddedSamplersInfo@ is a pointer to a
                                               -- 'BindDescriptorBufferEmbeddedSamplersInfoEXT' structure.
                                               (BindDescriptorBufferEmbeddedSamplersInfoEXT a)
                                            -> io ()
cmdBindDescriptorBufferEmbeddedSamplers2EXT commandBuffer
                                              bindDescriptorBufferEmbeddedSamplersInfo = liftIO . evalContT $ do
  let vkCmdBindDescriptorBufferEmbeddedSamplers2EXTPtr = pVkCmdBindDescriptorBufferEmbeddedSamplers2EXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindDescriptorBufferEmbeddedSamplers2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindDescriptorBufferEmbeddedSamplers2EXT is null" Nothing Nothing
  let vkCmdBindDescriptorBufferEmbeddedSamplers2EXT' = mkVkCmdBindDescriptorBufferEmbeddedSamplers2EXT vkCmdBindDescriptorBufferEmbeddedSamplers2EXTPtr
  pBindDescriptorBufferEmbeddedSamplersInfo <- ContT $ withCStruct (bindDescriptorBufferEmbeddedSamplersInfo)
  lift $ traceAroundEvent "vkCmdBindDescriptorBufferEmbeddedSamplers2EXT" (vkCmdBindDescriptorBufferEmbeddedSamplers2EXT'
                                                                             (commandBufferHandle (commandBuffer))
                                                                             (forgetExtensions pBindDescriptorBufferEmbeddedSamplersInfo))
  pure $ ()


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR"
pattern STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR = STRUCTURE_TYPE_BIND_MEMORY_STATUS


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR = STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR"
pattern STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR = STRUCTURE_TYPE_PUSH_CONSTANTS_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR"
pattern STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR = STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR"
pattern STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR = STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO


-- No documentation found for TopLevel "vkCmdBindDescriptorSets2KHR"
cmdBindDescriptorSets2KHR = cmdBindDescriptorSets2


-- No documentation found for TopLevel "vkCmdPushConstants2KHR"
cmdPushConstants2KHR = cmdPushConstants2


-- No documentation found for TopLevel "vkCmdPushDescriptorSet2KHR"
cmdPushDescriptorSet2KHR = cmdPushDescriptorSet2


-- No documentation found for TopLevel "vkCmdPushDescriptorSetWithTemplate2KHR"
cmdPushDescriptorSetWithTemplate2KHR = cmdPushDescriptorSetWithTemplate2


-- | VkSetDescriptorBufferOffsetsInfoEXT - Structure specifying descriptor
-- buffer offsets to set in a command buffer
--
-- = Description
--
-- If @stageFlags@ specifies a subset of all stages corresponding to one or
-- more pipeline bind points, the binding operation still affects all
-- stages corresponding to the given pipeline bind point(s) as if the
-- equivalent original version of this command had been called with the
-- same parameters. For example, specifying a @stageFlags@ value of
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' |
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT' |
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' is
-- equivalent to calling the original version of this command once with
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS' and
-- once with
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'.
--
-- == Valid Usage
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pOffsets-08061# The
--     offsets in @pOffsets@ /must/ be aligned to
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT'::@descriptorBufferOffsetAlignment@
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pOffsets-08063# The
--     offsets in @pOffsets@ /must/ be small enough such that any
--     descriptor binding referenced by @layout@ without the
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
--     flag computes a valid address inside the underlying
--     'Vulkan.Core10.Handles.Buffer'
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pOffsets-08126# The
--     offsets in @pOffsets@ /must/ be small enough such that any location
--     accessed by a shader as a sampler descriptor /must/ be within
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxSamplerDescriptorBufferRange@
--     of the sampler descriptor buffer binding
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pOffsets-08127# The
--     offsets in @pOffsets@ /must/ be small enough such that any location
--     accessed by a shader as a resource descriptor /must/ be within
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxResourceDescriptorBufferRange@
--     of the resource descriptor buffer binding
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pBufferIndices-08064# Each
--     element of @pBufferIndices@ /must/ be less than
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT'::@maxDescriptorBufferBindings@
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pBufferIndices-08065# Each
--     element of @pBufferIndices@ /must/ reference a valid descriptor
--     buffer binding set by a previous call to
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdBindDescriptorBuffersEXT'
--     in @commandBuffer@
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-firstSet-08066# The sum of
--     @firstSet@ and @setCount@ /must/ be less than or equal to
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-firstSet-09006# The
--     'Vulkan.Core10.Handles.DescriptorSetLayout' for each set from
--     @firstSet@ to @firstSet@ + @setCount@ when @layout@ was created
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--     bit set
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-firstSet-11803# The
--     'Vulkan.Core10.Handles.DescriptorSetLayout' for each set from
--     @firstSet@ to @firstSet@ + @setCount@ when @layout@ was created
--     /must/ not have been created with the
--     'Vulkan.Extensions.VK_KHR_push_descriptor.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--     bit set
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-firstSet-11804# The
--     'Vulkan.Core10.Handles.DescriptorSetLayout' for each set from
--     @firstSet@ to @firstSet@ + @setCount@ when @layout@ was created
--     /must/ not have been created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_EMBEDDED_IMMUTABLE_SAMPLERS_BIT_EXT'
--     bit set
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-layout-09496# If @layout@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@ chain
--     /must/ include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT'
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-stageFlags-parameter#
--     @stageFlags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-stageFlags-requiredbitmask#
--     @stageFlags@ /must/ not be @0@
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-layout-parameter# If
--     @layout@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pBufferIndices-parameter#
--     @pBufferIndices@ /must/ be a valid pointer to an array of @setCount@
--     @uint32_t@ values
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-pOffsets-parameter#
--     @pOffsets@ /must/ be a valid pointer to an array of @setCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-VkSetDescriptorBufferOffsetsInfoEXT-setCount-arraylength#
--     @setCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdSetDescriptorBufferOffsets2EXT'
data SetDescriptorBufferOffsetsInfoEXT (es :: [Type]) = SetDescriptorBufferOffsetsInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @stageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- the shader stages the descriptor sets will be bound to
    stageFlags :: ShaderStageFlags
  , -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
    -- program the bindings. If the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
    -- feature is enabled, @layout@ /can/ be
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the layout /must/ be
    -- specified by chaining
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure off
    -- the @pNext@
    layout :: PipelineLayout
  , -- | @firstSet@ is the number of the first set to be bound.
    firstSet :: Word32
  , -- | @pBufferIndices@ is a pointer to an array of indices into the descriptor
    -- buffer binding points set by
    -- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdBindDescriptorBuffersEXT'.
    bufferIndices :: Vector Word32
  , -- | @pOffsets@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' offsets to apply to the
    -- bound descriptor buffers.
    offsets :: Vector DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SetDescriptorBufferOffsetsInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SetDescriptorBufferOffsetsInfoEXT es)

instance Extensible SetDescriptorBufferOffsetsInfoEXT where
  extensibleTypeName = "SetDescriptorBufferOffsetsInfoEXT"
  setNext SetDescriptorBufferOffsetsInfoEXT{..} next' = SetDescriptorBufferOffsetsInfoEXT{next = next', ..}
  getNext SetDescriptorBufferOffsetsInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SetDescriptorBufferOffsetsInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss SetDescriptorBufferOffsetsInfoEXT es
         , PokeChain es ) => ToCStruct (SetDescriptorBufferOffsetsInfoEXT es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SetDescriptorBufferOffsetsInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (stageFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (firstSet)
    let pBufferIndicesLength = Data.Vector.length $ (bufferIndices)
    lift $ unless ((Data.Vector.length $ (offsets)) == pBufferIndicesLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBufferIndices must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral pBufferIndicesLength :: Word32))
    pPBufferIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (bufferIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (bufferIndices)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPBufferIndices')
    pPOffsets' <- ContT $ allocaBytes @DeviceSize ((Data.Vector.length (offsets)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets' `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr DeviceSize))) (pPOffsets')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    lift $ f

instance ( Extendss SetDescriptorBufferOffsetsInfoEXT es
         , PeekChain es ) => FromCStruct (SetDescriptorBufferOffsetsInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    layout <- peek @PipelineLayout ((p `plusPtr` 24 :: Ptr PipelineLayout))
    firstSet <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    setCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pBufferIndices <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pBufferIndices' <- generateM (fromIntegral setCount) (\i -> peek @Word32 ((pBufferIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pOffsets <- peek @(Ptr DeviceSize) ((p `plusPtr` 48 :: Ptr (Ptr DeviceSize)))
    pOffsets' <- generateM (fromIntegral setCount) (\i -> peek @DeviceSize ((pOffsets `advancePtrBytes` (8 * (i)) :: Ptr DeviceSize)))
    pure $ SetDescriptorBufferOffsetsInfoEXT
             next stageFlags layout firstSet pBufferIndices' pOffsets'

instance es ~ '[] => Zero (SetDescriptorBufferOffsetsInfoEXT es) where
  zero = SetDescriptorBufferOffsetsInfoEXT
           ()
           zero
           zero
           zero
           mempty
           mempty


-- | VkBindDescriptorBufferEmbeddedSamplersInfoEXT - Structure specifying
-- embedded immutable sampler offsets to set in a command buffer
--
-- = Description
--
-- If @stageFlags@ specifies a subset of all stages corresponding to one or
-- more pipeline bind points, the binding operation still affects all
-- stages corresponding to the given pipeline bind point(s) as if the
-- equivalent original version of this command had been called with the
-- same parameters. For example, specifying a @stageFlags@ value of
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' |
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT' |
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' is
-- equivalent to calling the original version of this command once with
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS' and
-- once with
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'.
--
-- == Valid Usage
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-set-08070# The
--     'Vulkan.Core10.Handles.DescriptorSetLayout' at index @set@ when
--     @layout@ was created /must/ have been created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_EMBEDDED_IMMUTABLE_SAMPLERS_BIT_EXT'
--     bit set
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-set-08071# @set@
--     /must/ be less than or equal to
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-None-09495# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-layout-09496# If
--     @layout@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@
--     chain /must/ include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT'
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-pNext-pNext#
--     @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-sType-unique#
--     The @sType@ value of each structure in the @pNext@ chain /must/ be
--     unique
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-stageFlags-parameter#
--     @stageFlags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-stageFlags-requiredbitmask#
--     @stageFlags@ /must/ not be @0@
--
-- -   #VUID-VkBindDescriptorBufferEmbeddedSamplersInfoEXT-layout-parameter#
--     If @layout@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBindDescriptorBufferEmbeddedSamplers2EXT'
data BindDescriptorBufferEmbeddedSamplersInfoEXT (es :: [Type]) = BindDescriptorBufferEmbeddedSamplersInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @stageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- the shader stages that will use the embedded immutable samplers.
    stageFlags :: ShaderStageFlags
  , -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
    -- program the bindings. If the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
    -- feature is enabled, @layout@ /can/ be
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the layout /must/ be
    -- specified by chaining
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure off
    -- the @pNext@
    layout :: PipelineLayout
  , -- | @set@ is the number of the set to be bound.
    set :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindDescriptorBufferEmbeddedSamplersInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindDescriptorBufferEmbeddedSamplersInfoEXT es)

instance Extensible BindDescriptorBufferEmbeddedSamplersInfoEXT where
  extensibleTypeName = "BindDescriptorBufferEmbeddedSamplersInfoEXT"
  setNext BindDescriptorBufferEmbeddedSamplersInfoEXT{..} next' = BindDescriptorBufferEmbeddedSamplersInfoEXT{next = next', ..}
  getNext BindDescriptorBufferEmbeddedSamplersInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindDescriptorBufferEmbeddedSamplersInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss BindDescriptorBufferEmbeddedSamplersInfoEXT es
         , PokeChain es ) => ToCStruct (BindDescriptorBufferEmbeddedSamplersInfoEXT es) where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindDescriptorBufferEmbeddedSamplersInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (stageFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (set)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    lift $ f

instance ( Extendss BindDescriptorBufferEmbeddedSamplersInfoEXT es
         , PeekChain es ) => FromCStruct (BindDescriptorBufferEmbeddedSamplersInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    layout <- peek @PipelineLayout ((p `plusPtr` 24 :: Ptr PipelineLayout))
    set <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ BindDescriptorBufferEmbeddedSamplersInfoEXT
             next stageFlags layout set

instance es ~ '[] => Zero (BindDescriptorBufferEmbeddedSamplersInfoEXT es) where
  zero = BindDescriptorBufferEmbeddedSamplersInfoEXT
           ()
           zero
           zero
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance6FeaturesKHR"
type PhysicalDeviceMaintenance6FeaturesKHR = PhysicalDeviceMaintenance6Features


-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance6PropertiesKHR"
type PhysicalDeviceMaintenance6PropertiesKHR = PhysicalDeviceMaintenance6Properties


-- No documentation found for TopLevel "VkBindMemoryStatusKHR"
type BindMemoryStatusKHR = BindMemoryStatus


-- No documentation found for TopLevel "VkBindDescriptorSetsInfoKHR"
type BindDescriptorSetsInfoKHR = BindDescriptorSetsInfo


-- No documentation found for TopLevel "VkPushConstantsInfoKHR"
type PushConstantsInfoKHR = PushConstantsInfo


-- No documentation found for TopLevel "VkPushDescriptorSetInfoKHR"
type PushDescriptorSetInfoKHR = PushDescriptorSetInfo


-- No documentation found for TopLevel "VkPushDescriptorSetWithTemplateInfoKHR"
type PushDescriptorSetWithTemplateInfoKHR = PushDescriptorSetWithTemplateInfo


type KHR_MAINTENANCE_6_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_6_SPEC_VERSION"
pattern KHR_MAINTENANCE_6_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_6_SPEC_VERSION = 1


type KHR_MAINTENANCE_6_EXTENSION_NAME = "VK_KHR_maintenance6"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_6_EXTENSION_NAME"
pattern KHR_MAINTENANCE_6_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_6_EXTENSION_NAME = "VK_KHR_maintenance6"

