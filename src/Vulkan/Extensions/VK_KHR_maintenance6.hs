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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_descriptor_buffer
--
--     -   Interacts with VK_KHR_push_descriptor
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is enabled, every index fetched results in a value of zero.
--
-- -   A new property @maxCombinedImageSamplerDescriptorCount@ to indicate
--     the maximum number of descriptors needed for any of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR'
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-08-01 (Jon Leech)
--
--     -   Initial revision
--
-- == See Also
--
-- 'BindDescriptorSetsInfoKHR', 'BindMemoryStatusKHR',
-- 'PhysicalDeviceMaintenance6FeaturesKHR',
-- 'PhysicalDeviceMaintenance6PropertiesKHR', 'PushConstantsInfoKHR',
-- 'cmdBindDescriptorSets2KHR', 'cmdPushConstants2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_maintenance6 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance6  ( cmdBindDescriptorSets2KHR
                                              , cmdPushConstants2KHR
                                              , cmdPushDescriptorSet2KHR
                                              , cmdPushDescriptorSetWithTemplate2KHR
                                              , cmdSetDescriptorBufferOffsets2EXT
                                              , cmdBindDescriptorBufferEmbeddedSamplers2EXT
                                              , PhysicalDeviceMaintenance6FeaturesKHR(..)
                                              , PhysicalDeviceMaintenance6PropertiesKHR(..)
                                              , BindMemoryStatusKHR(..)
                                              , BindDescriptorSetsInfoKHR(..)
                                              , PushConstantsInfoKHR(..)
                                              , PushDescriptorSetInfoKHR(..)
                                              , PushDescriptorSetWithTemplateInfoKHR(..)
                                              , SetDescriptorBufferOffsetsInfoEXT(..)
                                              , BindDescriptorBufferEmbeddedSamplersInfoEXT(..)
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
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (DescriptorSet)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorBufferEmbeddedSamplers2EXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorSets2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushConstants2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSet2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSetWithTemplate2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDescriptorBufferOffsets2EXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PipelineLayout)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PipelineLayoutCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.DescriptorSet (WriteDescriptorSet)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorSets2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct BindDescriptorSetsInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct BindDescriptorSetsInfoKHR) -> IO ()

-- | vkCmdBindDescriptorSets2KHR - Binds descriptor sets to a command buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindDescriptorSets2KHR-pBindDescriptorSetsInfo-09467#
--     Each bit in @pBindDescriptorSetsInfo->stageFlags@ /must/ be a stage
--     supported by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindDescriptorSets2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindDescriptorSets2KHR-pBindDescriptorSetsInfo-parameter#
--     @pBindDescriptorSetsInfo@ /must/ be a valid pointer to a valid
--     'BindDescriptorSetsInfoKHR' structure
--
-- -   #VUID-vkCmdBindDescriptorSets2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindDescriptorSets2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBindDescriptorSets2KHR-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'BindDescriptorSetsInfoKHR', 'Vulkan.Core10.Handles.CommandBuffer'
cmdBindDescriptorSets2KHR :: forall a io
                           . ( Extendss BindDescriptorSetsInfoKHR a
                             , PokeChain a
                             , MonadIO io )
                          => -- | @commandBuffer@ is the command buffer that the descriptor sets will be
                             -- bound to.
                             CommandBuffer
                          -> -- | @pBindDescriptorSetsInfo@ is a pointer to a 'BindDescriptorSetsInfoKHR'
                             -- structure.
                             (BindDescriptorSetsInfoKHR a)
                          -> io ()
cmdBindDescriptorSets2KHR commandBuffer
                            bindDescriptorSetsInfo = liftIO . evalContT $ do
  let vkCmdBindDescriptorSets2KHRPtr = pVkCmdBindDescriptorSets2KHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindDescriptorSets2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindDescriptorSets2KHR is null" Nothing Nothing
  let vkCmdBindDescriptorSets2KHR' = mkVkCmdBindDescriptorSets2KHR vkCmdBindDescriptorSets2KHRPtr
  pBindDescriptorSetsInfo <- ContT $ withCStruct (bindDescriptorSetsInfo)
  lift $ traceAroundEvent "vkCmdBindDescriptorSets2KHR" (vkCmdBindDescriptorSets2KHR'
                                                           (commandBufferHandle (commandBuffer))
                                                           (forgetExtensions pBindDescriptorSetsInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushConstants2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct PushConstantsInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct PushConstantsInfoKHR) -> IO ()

-- | vkCmdPushConstants2KHR - Update the values of push constants
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushConstants2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushConstants2KHR-pPushConstantsInfo-parameter#
--     @pPushConstantsInfo@ /must/ be a valid pointer to a valid
--     'PushConstantsInfoKHR' structure
--
-- -   #VUID-vkCmdPushConstants2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushConstants2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdPushConstants2KHR-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'PushConstantsInfoKHR'
cmdPushConstants2KHR :: forall a io
                      . ( Extendss PushConstantsInfoKHR a
                        , PokeChain a
                        , MonadIO io )
                     => -- | @commandBuffer@ is the command buffer in which the push constant update
                        -- will be recorded.
                        CommandBuffer
                     -> -- | @pPushConstantsInfo@ is a pointer to a 'PushConstantsInfoKHR' structure.
                        (PushConstantsInfoKHR a)
                     -> io ()
cmdPushConstants2KHR commandBuffer pushConstantsInfo = liftIO . evalContT $ do
  let vkCmdPushConstants2KHRPtr = pVkCmdPushConstants2KHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushConstants2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushConstants2KHR is null" Nothing Nothing
  let vkCmdPushConstants2KHR' = mkVkCmdPushConstants2KHR vkCmdPushConstants2KHRPtr
  pPushConstantsInfo <- ContT $ withCStruct (pushConstantsInfo)
  lift $ traceAroundEvent "vkCmdPushConstants2KHR" (vkCmdPushConstants2KHR'
                                                      (commandBufferHandle (commandBuffer))
                                                      (forgetExtensions pPushConstantsInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSet2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct PushDescriptorSetInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct PushDescriptorSetInfoKHR) -> IO ()

-- | vkCmdPushDescriptorSet2KHR - Pushes descriptor updates into a command
-- buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPushDescriptorSet2KHR-pPushDescriptorSetInfo-09468# Each
--     bit in @pPushDescriptorSetInfo->stageFlags@ /must/ be a stage
--     supported by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDescriptorSet2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDescriptorSet2KHR-pPushDescriptorSetInfo-parameter#
--     @pPushDescriptorSetInfo@ /must/ be a valid pointer to a valid
--     'PushDescriptorSetInfoKHR' structure
--
-- -   #VUID-vkCmdPushDescriptorSet2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDescriptorSet2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdPushDescriptorSet2KHR-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'PushDescriptorSetInfoKHR'
cmdPushDescriptorSet2KHR :: forall a io
                          . ( Extendss PushDescriptorSetInfoKHR a
                            , PokeChain a
                            , MonadIO io )
                         => -- | @commandBuffer@ is the command buffer that the descriptors will be
                            -- recorded in.
                            CommandBuffer
                         -> -- | @pPushDescriptorSetInfo@ is a pointer to a 'PushDescriptorSetInfoKHR'
                            -- structure.
                            (PushDescriptorSetInfoKHR a)
                         -> io ()
cmdPushDescriptorSet2KHR commandBuffer
                           pushDescriptorSetInfo = liftIO . evalContT $ do
  let vkCmdPushDescriptorSet2KHRPtr = pVkCmdPushDescriptorSet2KHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushDescriptorSet2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSet2KHR is null" Nothing Nothing
  let vkCmdPushDescriptorSet2KHR' = mkVkCmdPushDescriptorSet2KHR vkCmdPushDescriptorSet2KHRPtr
  pPushDescriptorSetInfo <- ContT $ withCStruct (pushDescriptorSetInfo)
  lift $ traceAroundEvent "vkCmdPushDescriptorSet2KHR" (vkCmdPushDescriptorSet2KHR'
                                                          (commandBufferHandle (commandBuffer))
                                                          (forgetExtensions pPushDescriptorSetInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetWithTemplate2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct PushDescriptorSetWithTemplateInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct PushDescriptorSetWithTemplateInfoKHR) -> IO ()

-- | vkCmdPushDescriptorSetWithTemplate2KHR - Pushes descriptor updates into
-- a command buffer using a descriptor update template
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2KHR-pPushDescriptorSetWithTemplateInfo-parameter#
--     @pPushDescriptorSetWithTemplateInfo@ /must/ be a valid pointer to a
--     valid 'PushDescriptorSetWithTemplateInfoKHR' structure
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2KHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2KHR-videocoding# This
--     command /must/ only be called outside of a video coding scope
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'PushDescriptorSetWithTemplateInfoKHR'
cmdPushDescriptorSetWithTemplate2KHR :: forall a io
                                      . ( Extendss PushDescriptorSetWithTemplateInfoKHR a
                                        , PokeChain a
                                        , MonadIO io )
                                     => -- | @commandBuffer@ is the command buffer that the descriptors will be
                                        -- recorded in.
                                        CommandBuffer
                                     -> -- | @pPushDescriptorSetWithTemplateInfo@ is a pointer to a
                                        -- 'PushDescriptorSetWithTemplateInfoKHR' structure.
                                        (PushDescriptorSetWithTemplateInfoKHR a)
                                     -> io ()
cmdPushDescriptorSetWithTemplate2KHR commandBuffer
                                       pushDescriptorSetWithTemplateInfo = liftIO . evalContT $ do
  let vkCmdPushDescriptorSetWithTemplate2KHRPtr = pVkCmdPushDescriptorSetWithTemplate2KHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushDescriptorSetWithTemplate2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSetWithTemplate2KHR is null" Nothing Nothing
  let vkCmdPushDescriptorSetWithTemplate2KHR' = mkVkCmdPushDescriptorSetWithTemplate2KHR vkCmdPushDescriptorSetWithTemplate2KHRPtr
  pPushDescriptorSetWithTemplateInfo <- ContT $ withCStruct (pushDescriptorSetWithTemplateInfo)
  lift $ traceAroundEvent "vkCmdPushDescriptorSetWithTemplate2KHR" (vkCmdPushDescriptorSetWithTemplate2KHR'
                                                                      (commandBufferHandle (commandBuffer))
                                                                      (forgetExtensions pPushDescriptorSetWithTemplateInfo))
  pure $ ()


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
-- -   #VUID-vkCmdSetDescriptorBufferOffsets2EXT-descriptorBuffer-09470#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
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
--     allocated from /must/ support graphics, or compute operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
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
-- -   #VUID-vkCmdBindDescriptorBufferEmbeddedSamplers2EXT-descriptorBuffer-09472#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
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
--     allocated from /must/ support graphics, or compute operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
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


-- | VkPhysicalDeviceMaintenance6FeaturesKHR - Structure describing whether
-- the implementation supports maintenance6 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance6FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMaintenance6FeaturesKHR' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance6FeaturesKHR = PhysicalDeviceMaintenance6FeaturesKHR
  { -- | #features-maintenance6# @maintenance6@ indicates that the implementation
    -- supports the following:
    --
    -- -   'Vulkan.Core10.APIConstants.NULL_HANDLE' /can/ be used when binding
    --     an index buffer
    --
    -- -   'BindMemoryStatusKHR' /can/ be included in the @pNext@ chain of the
    --     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
    --     and
    --     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo'
    --     structures, enabling applications to retrieve
    --     'Vulkan.Core10.Enums.Result.Result' values for individual memory
    --     binding operations.
    --
    -- -   'PhysicalDeviceMaintenance6PropertiesKHR'::@blockTexelViewCompatibleMultipleLayers@
    --     property to indicate that the implementation supports creating image
    --     views with
    --     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
    --     where the @layerCount@ member of @subresourceRange@ is greater than
    --     @1@.
    --
    -- -   'PhysicalDeviceMaintenance6PropertiesKHR'::@maxCombinedImageSamplerDescriptorCount@
    --     property which indicates the maximum descriptor size required for
    --     any
    --     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion format that requires a sampler Y′CBCR conversion>
    --     supported by the implementation.
    --
    -- -   A
    --     'PhysicalDeviceMaintenance6PropertiesKHR'::@fragmentShadingRateClampCombinerInputs@
    --     property which indicates whether the implementation clamps the
    --     inputs to fragment shading rate combiner operations.
    maintenance6 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance6FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance6FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance6FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance6FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance6))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance6FeaturesKHR where
  peekCStruct p = do
    maintenance6 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance6FeaturesKHR
             (bool32ToBool maintenance6)

instance Storable PhysicalDeviceMaintenance6FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance6FeaturesKHR where
  zero = PhysicalDeviceMaintenance6FeaturesKHR
           zero


-- | VkPhysicalDeviceMaintenance6PropertiesKHR - Structure describing various
-- implementation-defined properties introduced with VK_KHR_maintenance6
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance6PropertiesKHR' structure is included
-- in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance6PropertiesKHR = PhysicalDeviceMaintenance6PropertiesKHR
  { -- | @blockTexelViewCompatibleMultipleLayers@ is a boolean value indicating
    -- that an implementation supports creating image views with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
    -- where the @layerCount@ member of @subresourceRange@ is greater than @1@.
    blockTexelViewCompatibleMultipleLayers :: Bool
  , -- | @maxCombinedImageSamplerDescriptorCount@ is the maximum number of
    -- combined image sampler descriptors that the implementation uses to
    -- access any of the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>
    -- supported by the implementation.
    maxCombinedImageSamplerDescriptorCount :: Word32
  , -- | @fragmentShadingRateClampCombinerInputs@ is a boolean value indicating
    -- that an implementation clamps the inputs to
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-fragment-shading-rate-combining combiner operations>.
    fragmentShadingRateClampCombinerInputs :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance6PropertiesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance6PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance6PropertiesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance6PropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (blockTexelViewCompatibleMultipleLayers))
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxCombinedImageSamplerDescriptorCount)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateClampCombinerInputs))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance6PropertiesKHR where
  peekCStruct p = do
    blockTexelViewCompatibleMultipleLayers <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    maxCombinedImageSamplerDescriptorCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    fragmentShadingRateClampCombinerInputs <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance6PropertiesKHR
             (bool32ToBool blockTexelViewCompatibleMultipleLayers)
             maxCombinedImageSamplerDescriptorCount
             (bool32ToBool fragmentShadingRateClampCombinerInputs)

instance Storable PhysicalDeviceMaintenance6PropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance6PropertiesKHR where
  zero = PhysicalDeviceMaintenance6PropertiesKHR
           zero
           zero
           zero


-- | VkBindMemoryStatusKHR - Structure specifying where to return memory
-- binding status
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
-- or 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo'
-- includes a 'BindMemoryStatusKHR' structure, then the
-- 'BindMemoryStatusKHR'::@pResult@ will be populated with a value
-- describing the result of the corresponding memory binding operation.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.Enums.Result.Result',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BindMemoryStatusKHR = BindMemoryStatusKHR
  { -- | @pResult@ is a pointer to a 'Vulkan.Core10.Enums.Result.Result' value.
    --
    -- #VUID-VkBindMemoryStatusKHR-pResult-parameter# @pResult@ /must/ be a
    -- valid pointer to a 'Vulkan.Core10.Enums.Result.Result' value
    result :: Ptr Result }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindMemoryStatusKHR)
#endif
deriving instance Show BindMemoryStatusKHR

instance ToCStruct BindMemoryStatusKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindMemoryStatusKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Result))) (result)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Result))) (zero)
    f

instance FromCStruct BindMemoryStatusKHR where
  peekCStruct p = do
    pResult <- peek @(Ptr Result) ((p `plusPtr` 16 :: Ptr (Ptr Result)))
    pure $ BindMemoryStatusKHR
             pResult

instance Storable BindMemoryStatusKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindMemoryStatusKHR where
  zero = BindMemoryStatusKHR
           zero


-- | VkBindDescriptorSetsInfoKHR - Structure specifying a descriptor set
-- binding operation
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
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDescriptorSets-00358# Each
--     element of @pDescriptorSets@ /must/ have been allocated with a
--     'Vulkan.Core10.Handles.DescriptorSetLayout' that matches (is the
--     same as, or identically defined as) the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' at set /n/ in @layout@,
--     where /n/ is the sum of @firstSet@ and the index into
--     @pDescriptorSets@
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-dynamicOffsetCount-00359#
--     @dynamicOffsetCount@ /must/ be equal to the total number of dynamic
--     descriptors in @pDescriptorSets@
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-firstSet-00360# The sum of
--     @firstSet@ and @descriptorSetCount@ /must/ be less than or equal to
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDynamicOffsets-01971# Each
--     element of @pDynamicOffsets@ which corresponds to a descriptor
--     binding with type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minUniformBufferOffsetAlignment@
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDynamicOffsets-01972# Each
--     element of @pDynamicOffsets@ which corresponds to a descriptor
--     binding with type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minStorageBufferOffsetAlignment@
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDescriptorSets-01979# For each
--     dynamic uniform or storage buffer binding in @pDescriptorSets@, the
--     sum of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-effective-offset effective offset>
--     and the range of the binding /must/ be less than or equal to the
--     size of the buffer
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDescriptorSets-06715# For each
--     dynamic uniform or storage buffer binding in @pDescriptorSets@, if
--     the range was set with 'Vulkan.Core10.APIConstants.WHOLE_SIZE' then
--     @pDynamicOffsets@ which corresponds to the descriptor binding /must/
--     be 0
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDescriptorSets-04616# Each
--     element of @pDescriptorSets@ /must/ not have been allocated from a
--     'Vulkan.Core10.Handles.DescriptorPool' with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_EXT'
--     flag set
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDescriptorSets-06563# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-graphicsPipelineLibrary graphicsPipelineLibrary>
--     is not enabled, each element of @pDescriptorSets@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSet'
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDescriptorSets-08010# Each
--     element of @pDescriptorSets@ /must/ have been allocated with a
--     'Vulkan.Core10.Handles.DescriptorSetLayout' which was not created
--     with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-layout-09496# If @layout@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@ chain /must/
--     include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR'
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-stageFlags-parameter# @stageFlags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-stageFlags-requiredbitmask#
--     @stageFlags@ /must/ not be @0@
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-layout-parameter# If @layout@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ be a
--     valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDescriptorSets-parameter#
--     @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid 'Vulkan.Core10.Handles.DescriptorSet'
--     handles
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-pDynamicOffsets-parameter# If
--     @dynamicOffsetCount@ is not @0@, and @pDynamicOffsets@ is not
--     @NULL@, @pDynamicOffsets@ /must/ be a valid pointer to an array of
--     @dynamicOffsetCount@ or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     @uint32_t@ values
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-descriptorSetCount-arraylength#
--     @descriptorSetCount@ /must/ be greater than @0@
--
-- -   #VUID-VkBindDescriptorSetsInfoKHR-commonparent# Both of @layout@,
--     and the elements of @pDescriptorSets@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.Handles.DescriptorSet',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBindDescriptorSets2KHR'
data BindDescriptorSetsInfoKHR (es :: [Type]) = BindDescriptorSetsInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @stageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- the shader stages the descriptor sets will be bound to.
    stageFlags :: ShaderStageFlags
  , -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
    -- program the bindings. If the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
    -- feature is enabled, @layout@ /can/ be
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the layout /must/ be
    -- specified by chaining the
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure off
    -- the @pNext@
    layout :: PipelineLayout
  , -- | @firstSet@ is the set number of the first descriptor set to be bound.
    firstSet :: Word32
  , -- | @pDescriptorSets@ is a pointer to an array of handles to
    -- 'Vulkan.Core10.Handles.DescriptorSet' objects describing the descriptor
    -- sets to bind to.
    descriptorSets :: Vector DescriptorSet
  , -- | @dynamicOffsetCount@ is the number of dynamic offsets in the
    -- @pDynamicOffsets@ array.
    dynamicOffsetCount :: Word32
  , -- | @pDynamicOffsets@ is a pointer to an array of @uint32_t@ values
    -- specifying dynamic offsets.
    dynamicOffsets :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindDescriptorSetsInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindDescriptorSetsInfoKHR es)

instance Extensible BindDescriptorSetsInfoKHR where
  extensibleTypeName = "BindDescriptorSetsInfoKHR"
  setNext BindDescriptorSetsInfoKHR{..} next' = BindDescriptorSetsInfoKHR{next = next', ..}
  getNext BindDescriptorSetsInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindDescriptorSetsInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss BindDescriptorSetsInfoKHR es
         , PokeChain es ) => ToCStruct (BindDescriptorSetsInfoKHR es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindDescriptorSetsInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (stageFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (firstSet)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (descriptorSets)) :: Word32))
    pPDescriptorSets' <- ContT $ allocaBytes @DescriptorSet ((Data.Vector.length (descriptorSets)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorSets' `plusPtr` (8 * (i)) :: Ptr DescriptorSet) (e)) (descriptorSets)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr DescriptorSet))) (pPDescriptorSets')
    let pDynamicOffsetsLength = Data.Vector.length $ (dynamicOffsets)
    dynamicOffsetCount'' <- lift $ if (dynamicOffsetCount) == 0
      then pure $ fromIntegral pDynamicOffsetsLength
      else do
        unless (fromIntegral pDynamicOffsetsLength == (dynamicOffsetCount) || pDynamicOffsetsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pDynamicOffsets must be empty or have 'dynamicOffsetCount' elements" Nothing Nothing
        pure (dynamicOffsetCount)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (dynamicOffsetCount'')
    pDynamicOffsets'' <- if Data.Vector.null (dynamicOffsets)
      then pure nullPtr
      else do
        pPDynamicOffsets <- ContT $ allocaBytes @Word32 (((Data.Vector.length (dynamicOffsets))) * 4)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPDynamicOffsets `plusPtr` (4 * (i)) :: Ptr Word32) (e)) ((dynamicOffsets))
        pure $ pPDynamicOffsets
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Word32))) pDynamicOffsets''
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    lift $ f

instance ( Extendss BindDescriptorSetsInfoKHR es
         , PeekChain es ) => FromCStruct (BindDescriptorSetsInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    layout <- peek @PipelineLayout ((p `plusPtr` 24 :: Ptr PipelineLayout))
    firstSet <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    descriptorSetCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pDescriptorSets <- peek @(Ptr DescriptorSet) ((p `plusPtr` 40 :: Ptr (Ptr DescriptorSet)))
    pDescriptorSets' <- generateM (fromIntegral descriptorSetCount) (\i -> peek @DescriptorSet ((pDescriptorSets `advancePtrBytes` (8 * (i)) :: Ptr DescriptorSet)))
    dynamicOffsetCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pDynamicOffsets <- peek @(Ptr Word32) ((p `plusPtr` 56 :: Ptr (Ptr Word32)))
    let pDynamicOffsetsLength = if pDynamicOffsets == nullPtr then 0 else (fromIntegral dynamicOffsetCount)
    pDynamicOffsets' <- generateM pDynamicOffsetsLength (\i -> peek @Word32 ((pDynamicOffsets `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BindDescriptorSetsInfoKHR
             next
             stageFlags
             layout
             firstSet
             pDescriptorSets'
             dynamicOffsetCount
             pDynamicOffsets'

instance es ~ '[] => Zero (BindDescriptorSetsInfoKHR es) where
  zero = BindDescriptorSetsInfoKHR
           ()
           zero
           zero
           zero
           mempty
           zero
           mempty


-- | VkPushConstantsInfoKHR - Structure specifying a push constant update
-- operation
--
-- == Valid Usage
--
-- -   #VUID-VkPushConstantsInfoKHR-offset-01795# For each byte in the
--     range specified by @offset@ and @size@ and for each shader stage in
--     @stageFlags@, there /must/ be a push constant range in @layout@ that
--     includes that byte and that stage
--
-- -   #VUID-VkPushConstantsInfoKHR-offset-01796# For each byte in the
--     range specified by @offset@ and @size@ and for each push constant
--     range that overlaps that byte, @stageFlags@ /must/ include all
--     stages in that push constant range’s
--     'Vulkan.Core10.PipelineLayout.PushConstantRange'::@stageFlags@
--
-- -   #VUID-VkPushConstantsInfoKHR-offset-00368# @offset@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-VkPushConstantsInfoKHR-size-00369# @size@ /must/ be a multiple
--     of @4@
--
-- -   #VUID-VkPushConstantsInfoKHR-offset-00370# @offset@ /must/ be less
--     than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--
-- -   #VUID-VkPushConstantsInfoKHR-size-00371# @size@ /must/ be less than
--     or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--     minus @offset@
--
-- -   #VUID-VkPushConstantsInfoKHR-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushConstantsInfoKHR-layout-09496# If @layout@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@ chain /must/
--     include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPushConstantsInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR'
--
-- -   #VUID-VkPushConstantsInfoKHR-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkPushConstantsInfoKHR-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPushConstantsInfoKHR-layout-parameter# If @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushConstantsInfoKHR-stageFlags-parameter# @stageFlags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkPushConstantsInfoKHR-stageFlags-requiredbitmask#
--     @stageFlags@ /must/ not be @0@
--
-- -   #VUID-VkPushConstantsInfoKHR-pValues-parameter# @pValues@ /must/ be
--     a valid pointer to an array of @size@ bytes
--
-- -   #VUID-VkPushConstantsInfoKHR-size-arraylength# @size@ /must/ be
--     greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdPushConstants2KHR'
data PushConstantsInfoKHR (es :: [Type]) = PushConstantsInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @layout@ is the pipeline layout used to program the push constant
    -- updates. If the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
    -- feature is enabled, @layout@ /can/ be
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the layout /must/ be
    -- specified by chaining
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure off
    -- the @pNext@
    layout :: PipelineLayout
  , -- | @stageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- the shader stages that will use the push constants in the updated range.
    stageFlags :: ShaderStageFlags
  , -- | @offset@ is the start offset of the push constant range to update, in
    -- units of bytes.
    offset :: Word32
  , -- | @size@ is the size of the push constant range to update, in units of
    -- bytes.
    size :: Word32
  , -- | @pValues@ is a pointer to an array of @size@ bytes containing the new
    -- push constant values.
    values :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PushConstantsInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PushConstantsInfoKHR es)

instance Extensible PushConstantsInfoKHR where
  extensibleTypeName = "PushConstantsInfoKHR"
  setNext PushConstantsInfoKHR{..} next' = PushConstantsInfoKHR{next = next', ..}
  getNext PushConstantsInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PushConstantsInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss PushConstantsInfoKHR es
         , PokeChain es ) => ToCStruct (PushConstantsInfoKHR es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushConstantsInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 24 :: Ptr ShaderStageFlags)) (stageFlags)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (offset)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (size)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (values)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr ShaderStageFlags)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (zero)
    lift $ f

instance ( Extendss PushConstantsInfoKHR es
         , PeekChain es ) => FromCStruct (PushConstantsInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    layout <- peek @PipelineLayout ((p `plusPtr` 16 :: Ptr PipelineLayout))
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 24 :: Ptr ShaderStageFlags))
    offset <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    size <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pValues <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ PushConstantsInfoKHR
             next layout stageFlags offset size pValues

instance es ~ '[] => Zero (PushConstantsInfoKHR es) where
  zero = PushConstantsInfoKHR
           ()
           zero
           zero
           zero
           zero
           zero


-- | VkPushDescriptorSetInfoKHR - Structure specifying a descriptor set push
-- operation
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
-- -   #VUID-VkPushDescriptorSetInfoKHR-set-00364# @set@ /must/ be less
--     than
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-set-00365# @set@ /must/ be the
--     unique set number in the pipeline layout that uses a descriptor set
--     layout that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-pDescriptorWrites-06494# For each
--     element i where @pDescriptorWrites@[i].@descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     @pDescriptorWrites@[i].@pImageInfo@ /must/ be a valid pointer to an
--     array of @pDescriptorWrites@[i].@descriptorCount@ valid
--     'Vulkan.Core10.DescriptorSet.DescriptorImageInfo' structures
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-layout-09496# If @layout@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@ chain /must/
--     include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR'
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-stageFlags-parameter# @stageFlags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-stageFlags-requiredbitmask#
--     @stageFlags@ /must/ not be @0@
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-layout-parameter# If @layout@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ be a
--     valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-pDescriptorWrites-parameter#
--     @pDescriptorWrites@ /must/ be a valid pointer to an array of
--     @descriptorWriteCount@ valid
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet' structures
--
-- -   #VUID-VkPushDescriptorSetInfoKHR-descriptorWriteCount-arraylength#
--     @descriptorWriteCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'cmdPushDescriptorSet2KHR'
data PushDescriptorSetInfoKHR (es :: [Type]) = PushDescriptorSetInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @stageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- the shader stages that will use the descriptors.
    stageFlags :: ShaderStageFlags
  , -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
    -- program the bindings. If the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
    -- feature is enabled, @layout@ /can/ be
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the layout /must/ be
    -- specified by chaining
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure off
    -- the @pNext@
    layout :: PipelineLayout
  , -- | @set@ is the set number of the descriptor set in the pipeline layout
    -- that will be updated.
    set :: Word32
  , -- | @pDescriptorWrites@ is a pointer to an array of
    -- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet' structures describing
    -- the descriptors to be updated.
    descriptorWrites :: Vector (SomeStruct WriteDescriptorSet)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PushDescriptorSetInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PushDescriptorSetInfoKHR es)

instance Extensible PushDescriptorSetInfoKHR where
  extensibleTypeName = "PushDescriptorSetInfoKHR"
  setNext PushDescriptorSetInfoKHR{..} next' = PushDescriptorSetInfoKHR{next = next', ..}
  getNext PushDescriptorSetInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PushDescriptorSetInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss PushDescriptorSetInfoKHR es
         , PokeChain es ) => ToCStruct (PushDescriptorSetInfoKHR es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushDescriptorSetInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (stageFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (set)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (descriptorWrites)) :: Word32))
    pPDescriptorWrites' <- ContT $ allocaBytes @(WriteDescriptorSet _) ((Data.Vector.length (descriptorWrites)) * 64)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPDescriptorWrites' `plusPtr` (64 * (i)) :: Ptr (WriteDescriptorSet _))) (e) . ($ ())) (descriptorWrites)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (WriteDescriptorSet _)))) (pPDescriptorWrites')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    lift $ f

instance ( Extendss PushDescriptorSetInfoKHR es
         , PeekChain es ) => FromCStruct (PushDescriptorSetInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    layout <- peek @PipelineLayout ((p `plusPtr` 24 :: Ptr PipelineLayout))
    set <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    descriptorWriteCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pDescriptorWrites <- peek @(Ptr (WriteDescriptorSet _)) ((p `plusPtr` 40 :: Ptr (Ptr (WriteDescriptorSet _))))
    pDescriptorWrites' <- generateM (fromIntegral descriptorWriteCount) (\i -> peekSomeCStruct (forgetExtensions ((pDescriptorWrites `advancePtrBytes` (64 * (i)) :: Ptr (WriteDescriptorSet _)))))
    pure $ PushDescriptorSetInfoKHR
             next stageFlags layout set pDescriptorWrites'

instance es ~ '[] => Zero (PushDescriptorSetInfoKHR es) where
  zero = PushDescriptorSetInfoKHR
           ()
           zero
           zero
           zero
           mempty


-- | VkPushDescriptorSetWithTemplateInfoKHR - Structure specifying a
-- descriptor set push operation using a descriptor update template
--
-- == Valid Usage
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-commandBuffer-00366#
--     The @pipelineBindPoint@ specified during the creation of the
--     descriptor update template /must/ be supported by the
--     @commandBuffer@’s parent 'Vulkan.Core10.Handles.CommandPool'’s queue
--     family
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-pData-01686# @pData@
--     /must/ be a valid pointer to a memory containing one or more valid
--     instances of 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo', or
--     'Vulkan.Core10.Handles.BufferView' in a layout defined by
--     @descriptorUpdateTemplate@ when it was created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-layout-07993# @layout@
--     /must/ be compatible with the layout used to create
--     @descriptorUpdateTemplate@
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-descriptorUpdateTemplate-07994#
--     @descriptorUpdateTemplate@ /must/ have been created with a
--     @templateType@ of
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-set-07995# @set@ /must/
--     be the same value used to create @descriptorUpdateTemplate@
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-set-07304# @set@ /must/
--     be less than
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-set-07305# @set@ /must/
--     be the unique set number in the pipeline layout that uses a
--     descriptor set layout that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-layout-09496# If
--     @layout@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@
--     chain /must/ include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-sType-unique# The
--     @sType@ value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-descriptorUpdateTemplate-parameter#
--     @descriptorUpdateTemplate@ /must/ be a valid
--     'Vulkan.Core11.Handles.DescriptorUpdateTemplate' handle
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-layout-parameter# If
--     @layout@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-pData-parameter#
--     @pData@ /must/ be a pointer value
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfoKHR-commonparent# Both of
--     @descriptorUpdateTemplate@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- 'Vulkan.Core11.Handles.DescriptorUpdateTemplate',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdPushDescriptorSetWithTemplate2KHR'
data PushDescriptorSetWithTemplateInfoKHR (es :: [Type]) = PushDescriptorSetWithTemplateInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @descriptorUpdateTemplate@ is a descriptor update template defining how
    -- to interpret the descriptor information in @pData@.
    descriptorUpdateTemplate :: DescriptorUpdateTemplate
  , -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
    -- program the bindings. It /must/ be compatible with the layout used to
    -- create the @descriptorUpdateTemplate@ handle. If the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
    -- feature is enabled, @layout@ /can/ be
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the layout /must/ be
    -- specified by chaining
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure off
    -- the @pNext@
    layout :: PipelineLayout
  , -- | @set@ is the set number of the descriptor set in the pipeline layout
    -- that will be updated. This /must/ be the same number used to create the
    -- @descriptorUpdateTemplate@ handle.
    set :: Word32
  , -- | @pData@ is a pointer to memory containing descriptors for the templated
    -- update.
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PushDescriptorSetWithTemplateInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PushDescriptorSetWithTemplateInfoKHR es)

instance Extensible PushDescriptorSetWithTemplateInfoKHR where
  extensibleTypeName = "PushDescriptorSetWithTemplateInfoKHR"
  setNext PushDescriptorSetWithTemplateInfoKHR{..} next' = PushDescriptorSetWithTemplateInfoKHR{next = next', ..}
  getNext PushDescriptorSetWithTemplateInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PushDescriptorSetWithTemplateInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss PushDescriptorSetWithTemplateInfoKHR es
         , PokeChain es ) => ToCStruct (PushDescriptorSetWithTemplateInfoKHR es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushDescriptorSetWithTemplateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplate)) (descriptorUpdateTemplate)
    lift $ poke ((p `plusPtr` 24 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (set)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (data')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplate)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (zero)
    lift $ f

instance ( Extendss PushDescriptorSetWithTemplateInfoKHR es
         , PeekChain es ) => FromCStruct (PushDescriptorSetWithTemplateInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    descriptorUpdateTemplate <- peek @DescriptorUpdateTemplate ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplate))
    layout <- peek @PipelineLayout ((p `plusPtr` 24 :: Ptr PipelineLayout))
    set <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pData <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ PushDescriptorSetWithTemplateInfoKHR
             next descriptorUpdateTemplate layout set pData

instance es ~ '[] => Zero (PushDescriptorSetWithTemplateInfoKHR es) where
  zero = PushDescriptorSetWithTemplateInfoKHR
           ()
           zero
           zero
           zero
           zero


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
--     value of each struct in the @pNext@ chain /must/ be unique
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
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
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
--     The @sType@ value of each struct in the @pNext@ chain /must/ be
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
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
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


type KHR_MAINTENANCE_6_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_6_SPEC_VERSION"
pattern KHR_MAINTENANCE_6_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_6_SPEC_VERSION = 1


type KHR_MAINTENANCE_6_EXTENSION_NAME = "VK_KHR_maintenance6"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_6_EXTENSION_NAME"
pattern KHR_MAINTENANCE_6_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_6_EXTENSION_NAME = "VK_KHR_maintenance6"

