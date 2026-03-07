{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'"
module Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'  ( cmdBindDescriptorSets2
                                                                                , cmdPushConstants2
                                                                                , cmdPushDescriptorSet2
                                                                                , cmdPushDescriptorSetWithTemplate2
                                                                                , PhysicalDeviceMaintenance6Features(..)
                                                                                , PhysicalDeviceMaintenance6Properties(..)
                                                                                , BindMemoryStatus(..)
                                                                                , BindDescriptorSetsInfo(..)
                                                                                , PushConstantsInfo(..)
                                                                                , PushDescriptorSetInfo(..)
                                                                                , PushDescriptorSetWithTemplateInfo(..)
                                                                                , StructureType(..)
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
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorSets2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushConstants2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSet2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSetWithTemplate2))
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PipelineLayout)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PipelineLayoutCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_push_constant_bank (PushConstantBankInfoNV)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.DescriptorSet (WriteDescriptorSet)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_MEMORY_STATUS))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_CONSTANTS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorSets2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct BindDescriptorSetsInfo) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct BindDescriptorSetsInfo) -> IO ()

-- | vkCmdBindDescriptorSets2 - Binds descriptor sets to a command buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindDescriptorSets2-commandBuffer-11295# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdBindDescriptorSets2-commandBuffer-11296# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdBindDescriptorSets2-pBindDescriptorSetsInfo-09467# Each
--     bit in @pBindDescriptorSetsInfo->stageFlags@ /must/ be a stage
--     supported by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindDescriptorSets2-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindDescriptorSets2-pBindDescriptorSetsInfo-parameter#
--     @pBindDescriptorSetsInfo@ /must/ be a valid pointer to a valid
--     'BindDescriptorSetsInfo' structure
--
-- -   #VUID-vkCmdBindDescriptorSets2-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindDescriptorSets2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBindDescriptorSets2-videocoding# This command /must/ only
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBindDescriptorSets2 is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'BindDescriptorSetsInfo', 'Vulkan.Core10.Handles.CommandBuffer'
cmdBindDescriptorSets2 :: forall a io
                        . ( Extendss BindDescriptorSetsInfo a
                          , PokeChain a
                          , MonadIO io )
                       => -- | @commandBuffer@ is the command buffer that the descriptor sets will be
                          -- bound to.
                          CommandBuffer
                       -> -- | @pBindDescriptorSetsInfo@ is a pointer to a 'BindDescriptorSetsInfo'
                          -- structure.
                          (BindDescriptorSetsInfo a)
                       -> io ()
cmdBindDescriptorSets2 commandBuffer
                         bindDescriptorSetsInfo = liftIO . evalContT $ do
  let vkCmdBindDescriptorSets2Ptr = pVkCmdBindDescriptorSets2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindDescriptorSets2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindDescriptorSets2 is null" Nothing Nothing
  let vkCmdBindDescriptorSets2' = mkVkCmdBindDescriptorSets2 vkCmdBindDescriptorSets2Ptr
  pBindDescriptorSetsInfo <- ContT $ withCStruct (bindDescriptorSetsInfo)
  lift $ traceAroundEvent "vkCmdBindDescriptorSets2" (vkCmdBindDescriptorSets2'
                                                        (commandBufferHandle (commandBuffer))
                                                        (forgetExtensions pBindDescriptorSetsInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushConstants2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct PushConstantsInfo) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct PushConstantsInfo) -> IO ()

-- | vkCmdPushConstants2 - Update the values of push constants
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPushConstants2-commandBuffer-11295# If @commandBuffer@ is
--     a secondary command buffer, it /must/ have begun with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdPushConstants2-commandBuffer-11296# If @commandBuffer@ is
--     a secondary command buffer, it /must/ have begun with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushConstants2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushConstants2-pPushConstantsInfo-parameter#
--     @pPushConstantsInfo@ /must/ be a valid pointer to a valid
--     'PushConstantsInfo' structure
--
-- -   #VUID-vkCmdPushConstants2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushConstants2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdPushConstants2-videocoding# This command /must/ only be
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdPushConstants2 is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'PushConstantsInfo'
cmdPushConstants2 :: forall a io
                   . (Extendss PushConstantsInfo a, PokeChain a, MonadIO io)
                  => -- | @commandBuffer@ is the command buffer in which the push constant update
                     -- will be recorded.
                     CommandBuffer
                  -> -- | @pPushConstantsInfo@ is a pointer to a 'PushConstantsInfo' structure.
                     (PushConstantsInfo a)
                  -> io ()
cmdPushConstants2 commandBuffer pushConstantsInfo = liftIO . evalContT $ do
  let vkCmdPushConstants2Ptr = pVkCmdPushConstants2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushConstants2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushConstants2 is null" Nothing Nothing
  let vkCmdPushConstants2' = mkVkCmdPushConstants2 vkCmdPushConstants2Ptr
  pPushConstantsInfo <- ContT $ withCStruct (pushConstantsInfo)
  lift $ traceAroundEvent "vkCmdPushConstants2" (vkCmdPushConstants2'
                                                   (commandBufferHandle (commandBuffer))
                                                   (forgetExtensions pPushConstantsInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSet2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct PushDescriptorSetInfo) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct PushDescriptorSetInfo) -> IO ()

-- | vkCmdPushDescriptorSet2 - Pushes descriptor updates into a command
-- buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPushDescriptorSet2-commandBuffer-11295# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdPushDescriptorSet2-commandBuffer-11296# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdPushDescriptorSet2-pPushDescriptorSetInfo-09468# Each bit
--     in @pPushDescriptorSetInfo->stageFlags@ /must/ be a stage supported
--     by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-vkCmdPushDescriptorSet2-None-10357# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
--     extension is not enabled,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pushDescriptor pushDescriptor>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDescriptorSet2-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDescriptorSet2-pPushDescriptorSetInfo-parameter#
--     @pPushDescriptorSetInfo@ /must/ be a valid pointer to a valid
--     'PushDescriptorSetInfo' structure
--
-- -   #VUID-vkCmdPushDescriptorSet2-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDescriptorSet2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdPushDescriptorSet2-videocoding# This command /must/ only
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdPushDescriptorSet2 is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'PushDescriptorSetInfo'
cmdPushDescriptorSet2 :: forall a io
                       . ( Extendss PushDescriptorSetInfo a
                         , PokeChain a
                         , MonadIO io )
                      => -- | @commandBuffer@ is the command buffer that the descriptors will be
                         -- recorded in.
                         CommandBuffer
                      -> -- | @pPushDescriptorSetInfo@ is a pointer to a 'PushDescriptorSetInfo'
                         -- structure.
                         (PushDescriptorSetInfo a)
                      -> io ()
cmdPushDescriptorSet2 commandBuffer
                        pushDescriptorSetInfo = liftIO . evalContT $ do
  let vkCmdPushDescriptorSet2Ptr = pVkCmdPushDescriptorSet2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushDescriptorSet2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSet2 is null" Nothing Nothing
  let vkCmdPushDescriptorSet2' = mkVkCmdPushDescriptorSet2 vkCmdPushDescriptorSet2Ptr
  pPushDescriptorSetInfo <- ContT $ withCStruct (pushDescriptorSetInfo)
  lift $ traceAroundEvent "vkCmdPushDescriptorSet2" (vkCmdPushDescriptorSet2'
                                                       (commandBufferHandle (commandBuffer))
                                                       (forgetExtensions pPushDescriptorSetInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetWithTemplate2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct PushDescriptorSetWithTemplateInfo) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct PushDescriptorSetWithTemplateInfo) -> IO ()

-- | vkCmdPushDescriptorSetWithTemplate2 - Pushes descriptor updates into a
-- command buffer using a descriptor update template
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2-commandBuffer-11295# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2-commandBuffer-11296# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2-pPushDescriptorSetWithTemplateInfo-parameter#
--     @pPushDescriptorSetWithTemplateInfo@ /must/ be a valid pointer to a
--     valid 'PushDescriptorSetWithTemplateInfo' structure
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate2-videocoding# This command
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdPushDescriptorSetWithTemplate2 is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'PushDescriptorSetWithTemplateInfo'
cmdPushDescriptorSetWithTemplate2 :: forall a io
                                   . ( Extendss PushDescriptorSetWithTemplateInfo a
                                     , PokeChain a
                                     , MonadIO io )
                                  => -- | @commandBuffer@ is the command buffer that the descriptors will be
                                     -- recorded in.
                                     CommandBuffer
                                  -> -- | @pPushDescriptorSetWithTemplateInfo@ is a pointer to a
                                     -- 'PushDescriptorSetWithTemplateInfo' structure.
                                     (PushDescriptorSetWithTemplateInfo a)
                                  -> io ()
cmdPushDescriptorSetWithTemplate2 commandBuffer
                                    pushDescriptorSetWithTemplateInfo = liftIO . evalContT $ do
  let vkCmdPushDescriptorSetWithTemplate2Ptr = pVkCmdPushDescriptorSetWithTemplate2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushDescriptorSetWithTemplate2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSetWithTemplate2 is null" Nothing Nothing
  let vkCmdPushDescriptorSetWithTemplate2' = mkVkCmdPushDescriptorSetWithTemplate2 vkCmdPushDescriptorSetWithTemplate2Ptr
  pPushDescriptorSetWithTemplateInfo <- ContT $ withCStruct (pushDescriptorSetWithTemplateInfo)
  lift $ traceAroundEvent "vkCmdPushDescriptorSetWithTemplate2" (vkCmdPushDescriptorSetWithTemplate2'
                                                                   (commandBufferHandle (commandBuffer))
                                                                   (forgetExtensions pPushDescriptorSetWithTemplateInfo))
  pure $ ()


-- | VkPhysicalDeviceMaintenance6Features - Structure describing whether the
-- implementation supports maintenance6 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance6Features' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceMaintenance6Features', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance6Features = PhysicalDeviceMaintenance6Features
  { -- | #extension-features-maintenance6# @maintenance6@ indicates that the
    -- implementation supports the following:
    --
    -- -   'Vulkan.Core10.APIConstants.NULL_HANDLE' /can/ be used when binding
    --     an index buffer
    --
    -- -   'BindMemoryStatus' /can/ be included in the @pNext@ chain of the
    --     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
    --     and
    --     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo'
    --     structures, enabling applications to retrieve
    --     'Vulkan.Core10.Enums.Result.Result' values for individual memory
    --     binding operations.
    --
    -- -   'PhysicalDeviceMaintenance6Properties'::@blockTexelViewCompatibleMultipleLayers@
    --     property to indicate that the implementation supports creating image
    --     views with
    --     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
    --     where the @layerCount@ member of @subresourceRange@ is greater than
    --     @1@.
    --
    -- -   'PhysicalDeviceMaintenance6Properties'::@maxCombinedImageSamplerDescriptorCount@
    --     property which indicates the maximum descriptor size required for
    --     any
    --     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion format that requires a sampler Y′CBCR conversion>
    --     supported by the implementation.
    --
    -- -   A
    --     'PhysicalDeviceMaintenance6Properties'::@fragmentShadingRateClampCombinerInputs@
    --     property which indicates whether the implementation clamps the
    --     inputs to fragment shading rate combiner operations.
    maintenance6 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance6Features)
#endif
deriving instance Show PhysicalDeviceMaintenance6Features

instance ToCStruct PhysicalDeviceMaintenance6Features where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance6Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance6))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance6Features where
  peekCStruct p = do
    maintenance6 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance6Features
             (bool32ToBool maintenance6)

instance Storable PhysicalDeviceMaintenance6Features where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance6Features where
  zero = PhysicalDeviceMaintenance6Features
           zero


-- | VkPhysicalDeviceMaintenance6Properties - Structure describing various
-- implementation-defined properties introduced with VK_KHR_maintenance6
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance6Properties' structure is included in
-- the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance6Properties = PhysicalDeviceMaintenance6Properties
  { -- | @blockTexelViewCompatibleMultipleLayers@ is a boolean value indicating
    -- that an implementation supports creating image views with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
    -- where the @layerCount@ member of @subresourceRange@ is greater than @1@.
    blockTexelViewCompatibleMultipleLayers :: Bool
  , -- | @maxCombinedImageSamplerDescriptorCount@ is the maximum number of
    -- combined image sampler descriptors that the implementation uses to
    -- access any of the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>
    -- supported by the implementation.
    maxCombinedImageSamplerDescriptorCount :: Word32
  , -- | @fragmentShadingRateClampCombinerInputs@ is a boolean value indicating
    -- that an implementation clamps the inputs to
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-fragment-shading-rate-combining combiner operations>.
    fragmentShadingRateClampCombinerInputs :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance6Properties)
#endif
deriving instance Show PhysicalDeviceMaintenance6Properties

instance ToCStruct PhysicalDeviceMaintenance6Properties where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance6Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (blockTexelViewCompatibleMultipleLayers))
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxCombinedImageSamplerDescriptorCount)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateClampCombinerInputs))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance6Properties where
  peekCStruct p = do
    blockTexelViewCompatibleMultipleLayers <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    maxCombinedImageSamplerDescriptorCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    fragmentShadingRateClampCombinerInputs <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance6Properties
             (bool32ToBool blockTexelViewCompatibleMultipleLayers)
             maxCombinedImageSamplerDescriptorCount
             (bool32ToBool fragmentShadingRateClampCombinerInputs)

instance Storable PhysicalDeviceMaintenance6Properties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance6Properties where
  zero = PhysicalDeviceMaintenance6Properties
           zero
           zero
           zero


-- | VkBindMemoryStatus - Structure specifying where to return memory binding
-- status
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
-- or 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo'
-- includes a 'BindMemoryStatus' structure, then the
-- 'BindMemoryStatus'::@pResult@ will be populated with a value describing
-- the result of the corresponding memory binding operation.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Enums.Result.Result',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BindMemoryStatus = BindMemoryStatus
  { -- | @pResult@ is a pointer to a 'Vulkan.Core10.Enums.Result.Result' value.
    --
    -- #VUID-VkBindMemoryStatus-pResult-parameter# @pResult@ /must/ be a valid
    -- pointer to a 'Vulkan.Core10.Enums.Result.Result' value
    result :: Ptr Result }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindMemoryStatus)
#endif
deriving instance Show BindMemoryStatus

instance ToCStruct BindMemoryStatus where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindMemoryStatus{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_MEMORY_STATUS)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Result))) (result)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_MEMORY_STATUS)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Result))) (zero)
    f

instance FromCStruct BindMemoryStatus where
  peekCStruct p = do
    pResult <- peek @(Ptr Result) ((p `plusPtr` 16 :: Ptr (Ptr Result)))
    pure $ BindMemoryStatus
             pResult

instance Storable BindMemoryStatus where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindMemoryStatus where
  zero = BindMemoryStatus
           zero


-- | VkBindDescriptorSetsInfo - Structure specifying a descriptor set binding
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
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-00358# Each element
--     of @pDescriptorSets@ that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been allocated
--     with a 'Vulkan.Core10.Handles.DescriptorSetLayout' that matches (is
--     the same as, or identically defined as) the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' at set /n/ in @layout@,
--     where /n/ is the sum of @firstSet@ and the index into
--     @pDescriptorSets@
--
-- -   #VUID-VkBindDescriptorSetsInfo-dynamicOffsetCount-00359#
--     @dynamicOffsetCount@ /must/ be equal to the total number of dynamic
--     descriptors in @pDescriptorSets@
--
-- -   #VUID-VkBindDescriptorSetsInfo-firstSet-00360# The sum of @firstSet@
--     and @descriptorSetCount@ /must/ be less than or equal to
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDynamicOffsets-01971# Each element
--     of @pDynamicOffsets@ which corresponds to a descriptor binding with
--     type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minUniformBufferOffsetAlignment@
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDynamicOffsets-01972# Each element
--     of @pDynamicOffsets@ which corresponds to a descriptor binding with
--     type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minStorageBufferOffsetAlignment@
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-01979# For each
--     dynamic uniform or storage buffer binding in @pDescriptorSets@, the
--     sum of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-effective-offset effective offset>
--     and the range of the binding /must/ be less than or equal to the
--     size of the buffer
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-06715# For each
--     dynamic uniform or storage buffer binding in @pDescriptorSets@, if
--     the range was set with 'Vulkan.Core10.APIConstants.WHOLE_SIZE' then
--     @pDynamicOffsets@ which corresponds to the descriptor binding /must/
--     be 0
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-04616# Each element
--     of @pDescriptorSets@ /must/ not have been allocated from a
--     'Vulkan.Core10.Handles.DescriptorPool' with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_EXT'
--     flag set
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-06563# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-graphicsPipelineLibrary graphicsPipelineLibrary>
--     feature is not enabled, each element of @pDescriptorSets@ /must/ be
--     a valid 'Vulkan.Core10.Handles.DescriptorSet'
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-08010# Each element
--     of @pDescriptorSets@ /must/ have been allocated with a
--     'Vulkan.Core10.Handles.DescriptorSetLayout' which was not created
--     with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-09914# If any element
--     of @pDescriptorSets@ was allocated from a descriptor pool created
--     with a 'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo'
--     structure that had a
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphProcessingEngineCreateInfoARM'
--     structure specifying foreign data processing engines in its @pNext@
--     chain, then the command pool from which @commandBuffer@ was
--     allocated /must/ have been created with a
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that had
--     a
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphProcessingEngineCreateInfoARM'
--     structure in its @pNext@ chain specifying a superset of all the
--     foreign data processing engines specified when creating the
--     descriptor pools from which the elements of @pDescriptorSets@ were
--     allocated
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-09915# If none of the
--     elements of @pDescriptorSets@ were allocated from a descriptor pool
--     created with a
--     'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo' structure
--     that had a
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphProcessingEngineCreateInfoARM'
--     structure specifying foreign data processing engines in its @pNext@
--     chain, then the command pool from which @commandBuffer@ was
--     allocated /must/ not have been created with a
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that had
--     a
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphProcessingEngineCreateInfoARM'
--     structure in its @pNext@ chain
--
-- -   #VUID-VkBindDescriptorSetsInfo-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkBindDescriptorSetsInfo-layout-09496# If @layout@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@ chain /must/
--     include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindDescriptorSetsInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO'
--
-- -   #VUID-VkBindDescriptorSetsInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkBindDescriptorSetsInfo-sType-unique# The @sType@ value of
--     each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBindDescriptorSetsInfo-stageFlags-parameter# @stageFlags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkBindDescriptorSetsInfo-stageFlags-requiredbitmask#
--     @stageFlags@ /must/ not be @0@
--
-- -   #VUID-VkBindDescriptorSetsInfo-layout-parameter# If @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDescriptorSets-parameter#
--     @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid 'Vulkan.Core10.Handles.DescriptorSet'
--     handles
--
-- -   #VUID-VkBindDescriptorSetsInfo-pDynamicOffsets-parameter# If
--     @dynamicOffsetCount@ is not @0@, and @pDynamicOffsets@ is not
--     @NULL@, @pDynamicOffsets@ /must/ be a valid pointer to an array of
--     @dynamicOffsetCount@ or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     @uint32_t@ values
--
-- -   #VUID-VkBindDescriptorSetsInfo-descriptorSetCount-arraylength#
--     @descriptorSetCount@ /must/ be greater than @0@
--
-- -   #VUID-VkBindDescriptorSetsInfo-commonparent# Both of @layout@, and
--     the elements of @pDescriptorSets@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.DescriptorSet',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBindDescriptorSets2', 'cmdBindDescriptorSets2'
data BindDescriptorSetsInfo (es :: [Type]) = BindDescriptorSetsInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @stageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- the shader stages the descriptor sets will be bound to.
    stageFlags :: ShaderStageFlags
  , -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
    -- program the bindings. If the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
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
deriving instance Generic (BindDescriptorSetsInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindDescriptorSetsInfo es)

instance Extensible BindDescriptorSetsInfo where
  extensibleTypeName = "BindDescriptorSetsInfo"
  setNext BindDescriptorSetsInfo{..} next' = BindDescriptorSetsInfo{next = next', ..}
  getNext BindDescriptorSetsInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindDescriptorSetsInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss BindDescriptorSetsInfo es
         , PokeChain es ) => ToCStruct (BindDescriptorSetsInfo es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindDescriptorSetsInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    lift $ f

instance ( Extendss BindDescriptorSetsInfo es
         , PeekChain es ) => FromCStruct (BindDescriptorSetsInfo es) where
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
    pure $ BindDescriptorSetsInfo
             next
             stageFlags
             layout
             firstSet
             pDescriptorSets'
             dynamicOffsetCount
             pDynamicOffsets'

instance es ~ '[] => Zero (BindDescriptorSetsInfo es) where
  zero = BindDescriptorSetsInfo
           ()
           zero
           zero
           zero
           mempty
           zero
           mempty


-- | VkPushConstantsInfo - Structure specifying a push constant update
-- operation
--
-- == Valid Usage
--
-- -   #VUID-VkPushConstantsInfo-offset-01795# For each byte in the range
--     specified by @offset@ and @size@ and for each shader stage in
--     @stageFlags@, there /must/ be a push constant range in @layout@ that
--     includes that byte and that stage
--
-- -   #VUID-VkPushConstantsInfo-offset-01796# For each byte in the range
--     specified by @offset@ and @size@ and for each push constant range
--     that overlaps that byte, @stageFlags@ /must/ include all stages in
--     that push constant range’s
--     'Vulkan.Core10.PipelineLayout.PushConstantRange'::@stageFlags@
--
-- -   #VUID-VkPushConstantsInfo-offset-00368# @offset@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-VkPushConstantsInfo-size-00369# @size@ /must/ be a multiple of
--     @4@
--
-- -   #VUID-VkPushConstantsInfo-offset-00370# @offset@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--
-- -   #VUID-VkPushConstantsInfo-size-00371# @size@ /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--     minus @offset@
--
-- -   #VUID-VkPushConstantsInfo-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushConstantsInfo-layout-09496# If @layout@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@ chain /must/
--     include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPushConstantsInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_CONSTANTS_INFO'
--
-- -   #VUID-VkPushConstantsInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' or
--     'Vulkan.Extensions.VK_NV_push_constant_bank.PushConstantBankInfoNV'
--
-- -   #VUID-VkPushConstantsInfo-sType-unique# The @sType@ value of each
--     structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPushConstantsInfo-layout-parameter# If @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushConstantsInfo-stageFlags-parameter# @stageFlags@ /must/
--     be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkPushConstantsInfo-stageFlags-requiredbitmask# @stageFlags@
--     /must/ not be @0@
--
-- -   #VUID-VkPushConstantsInfo-pValues-parameter# @pValues@ /must/ be a
--     valid pointer to an array of @size@ bytes
--
-- -   #VUID-VkPushConstantsInfo-size-arraylength# @size@ /must/ be greater
--     than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdPushConstants2',
-- 'cmdPushConstants2'
data PushConstantsInfo (es :: [Type]) = PushConstantsInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @layout@ is the pipeline layout used to program the push constant
    -- updates. If the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
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
deriving instance Generic (PushConstantsInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PushConstantsInfo es)

instance Extensible PushConstantsInfo where
  extensibleTypeName = "PushConstantsInfo"
  setNext PushConstantsInfo{..} next' = PushConstantsInfo{next = next', ..}
  getNext PushConstantsInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PushConstantsInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PushConstantBankInfoNV = Just f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss PushConstantsInfo es
         , PokeChain es ) => ToCStruct (PushConstantsInfo es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushConstantsInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_CONSTANTS_INFO)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_CONSTANTS_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr ShaderStageFlags)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (zero)
    lift $ f

instance ( Extendss PushConstantsInfo es
         , PeekChain es ) => FromCStruct (PushConstantsInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    layout <- peek @PipelineLayout ((p `plusPtr` 16 :: Ptr PipelineLayout))
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 24 :: Ptr ShaderStageFlags))
    offset <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    size <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pValues <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ PushConstantsInfo
             next layout stageFlags offset size pValues

instance es ~ '[] => Zero (PushConstantsInfo es) where
  zero = PushConstantsInfo
           ()
           zero
           zero
           zero
           zero
           zero


-- | VkPushDescriptorSetInfo - Structure specifying a descriptor set push
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
-- -   #VUID-VkPushDescriptorSetInfo-set-00364# @set@ /must/ be less than
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-VkPushDescriptorSetInfo-set-00365# @set@ /must/ be the unique
--     set number in the pipeline layout that uses a descriptor set layout
--     that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT'
--
-- -   #VUID-VkPushDescriptorSetInfo-pDescriptorWrites-06494# For each
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
-- -   #VUID-VkPushDescriptorSetInfo-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushDescriptorSetInfo-layout-09496# If @layout@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@ chain /must/
--     include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPushDescriptorSetInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO'
--
-- -   #VUID-VkPushDescriptorSetInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkPushDescriptorSetInfo-sType-unique# The @sType@ value of
--     each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPushDescriptorSetInfo-stageFlags-parameter# @stageFlags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkPushDescriptorSetInfo-stageFlags-requiredbitmask#
--     @stageFlags@ /must/ not be @0@
--
-- -   #VUID-VkPushDescriptorSetInfo-layout-parameter# If @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushDescriptorSetInfo-pDescriptorWrites-parameter#
--     @pDescriptorWrites@ /must/ be a valid pointer to an array of
--     @descriptorWriteCount@ valid
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet' structures
--
-- -   #VUID-VkPushDescriptorSetInfo-descriptorWriteCount-arraylength#
--     @descriptorWriteCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'cmdPushDescriptorSet2', 'cmdPushDescriptorSet2'
data PushDescriptorSetInfo (es :: [Type]) = PushDescriptorSetInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @stageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- the shader stages that will use the descriptors.
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
deriving instance Generic (PushDescriptorSetInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PushDescriptorSetInfo es)

instance Extensible PushDescriptorSetInfo where
  extensibleTypeName = "PushDescriptorSetInfo"
  setNext PushDescriptorSetInfo{..} next' = PushDescriptorSetInfo{next = next', ..}
  getNext PushDescriptorSetInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PushDescriptorSetInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss PushDescriptorSetInfo es
         , PokeChain es ) => ToCStruct (PushDescriptorSetInfo es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushDescriptorSetInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    lift $ f

instance ( Extendss PushDescriptorSetInfo es
         , PeekChain es ) => FromCStruct (PushDescriptorSetInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    layout <- peek @PipelineLayout ((p `plusPtr` 24 :: Ptr PipelineLayout))
    set <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    descriptorWriteCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pDescriptorWrites <- peek @(Ptr (WriteDescriptorSet _)) ((p `plusPtr` 40 :: Ptr (Ptr (WriteDescriptorSet _))))
    pDescriptorWrites' <- generateM (fromIntegral descriptorWriteCount) (\i -> peekSomeCStruct (forgetExtensions ((pDescriptorWrites `advancePtrBytes` (64 * (i)) :: Ptr (WriteDescriptorSet _)))))
    pure $ PushDescriptorSetInfo
             next stageFlags layout set pDescriptorWrites'

instance es ~ '[] => Zero (PushDescriptorSetInfo es) where
  zero = PushDescriptorSetInfo
           ()
           zero
           zero
           zero
           mempty


-- | VkPushDescriptorSetWithTemplateInfo - Structure specifying a descriptor
-- set push operation using a descriptor update template
--
-- == Valid Usage
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-commandBuffer-00366# The
--     @pipelineBindPoint@ specified during the creation of the descriptor
--     update template /must/ be supported by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-pData-01686# @pData@
--     /must/ be a valid pointer to a memory containing one or more valid
--     instances of 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo', or
--     'Vulkan.Core10.Handles.BufferView' in a layout defined by
--     @descriptorUpdateTemplate@ when it was created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-layout-07993# @layout@
--     /must/ be compatible with the layout used to create
--     @descriptorUpdateTemplate@
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-descriptorUpdateTemplate-07994#
--     @descriptorUpdateTemplate@ /must/ have been created with a
--     @templateType@ of
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-set-07995# @set@ /must/ be
--     the same value used to create @descriptorUpdateTemplate@
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-set-07304# @set@ /must/ be
--     less than
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-set-11854# @set@ /must/
--     reference a valid 'Vulkan.Core10.Handles.DescriptorSetLayout' handle
--     in @layout@
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-set-07305# @set@ /must/ be
--     the unique set number in the pipeline layout that uses a descriptor
--     set layout that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-None-09495# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
--     feature is not enabled, @layout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-layout-09496# If @layout@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the @pNext@ chain
--     /must/ include a valid
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-None-10359# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
--     extension is not enabled,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pushDescriptor pushDescriptor>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-descriptorUpdateTemplate-parameter#
--     @descriptorUpdateTemplate@ /must/ be a valid
--     'Vulkan.Core11.Handles.DescriptorUpdateTemplate' handle
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-layout-parameter# If
--     @layout@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-pData-parameter# @pData@
--     /must/ be a pointer value
--
-- -   #VUID-VkPushDescriptorSetWithTemplateInfo-commonparent# Both of
--     @descriptorUpdateTemplate@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core11.Handles.DescriptorUpdateTemplate',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdPushDescriptorSetWithTemplate2', 'cmdPushDescriptorSetWithTemplate2'
data PushDescriptorSetWithTemplateInfo (es :: [Type]) = PushDescriptorSetWithTemplateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @descriptorUpdateTemplate@ is a descriptor update template defining how
    -- to interpret the descriptor information in @pData@.
    descriptorUpdateTemplate :: DescriptorUpdateTemplate
  , -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
    -- program the bindings. It /must/ be compatible with the layout used to
    -- create the @descriptorUpdateTemplate@ handle. If the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicPipelineLayout dynamicPipelineLayout>
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
deriving instance Generic (PushDescriptorSetWithTemplateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PushDescriptorSetWithTemplateInfo es)

instance Extensible PushDescriptorSetWithTemplateInfo where
  extensibleTypeName = "PushDescriptorSetWithTemplateInfo"
  setNext PushDescriptorSetWithTemplateInfo{..} next' = PushDescriptorSetWithTemplateInfo{next = next', ..}
  getNext PushDescriptorSetWithTemplateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PushDescriptorSetWithTemplateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss PushDescriptorSetWithTemplateInfo es
         , PokeChain es ) => ToCStruct (PushDescriptorSetWithTemplateInfo es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushDescriptorSetWithTemplateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplate)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (zero)
    lift $ f

instance ( Extendss PushDescriptorSetWithTemplateInfo es
         , PeekChain es ) => FromCStruct (PushDescriptorSetWithTemplateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    descriptorUpdateTemplate <- peek @DescriptorUpdateTemplate ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplate))
    layout <- peek @PipelineLayout ((p `plusPtr` 24 :: Ptr PipelineLayout))
    set <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pData <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ PushDescriptorSetWithTemplateInfo
             next descriptorUpdateTemplate layout set pData

instance es ~ '[] => Zero (PushDescriptorSetWithTemplateInfo es) where
  zero = PushDescriptorSetWithTemplateInfo
           ()
           zero
           zero
           zero
           zero

