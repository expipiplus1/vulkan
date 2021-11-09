{-# language CPP #-}
-- | = Name
--
-- VK_KHR_dynamic_rendering - device extension
--
-- == VK_KHR_dynamic_rendering
--
-- [__Name String__]
--     @VK_KHR_dynamic_rendering@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     45
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_dynamic_rendering] @tobski%0A<<Here describe the issue or question you have about the VK_KHR_dynamic_rendering extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_dynamic_rendering.asciidoc VK_KHR_dynamic_rendering>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-06
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Arseny Kapoulkine, Roblox
--
--     -   François Duranleau, Gameloft
--
--     -   Stuart Smith, AMD
--
--     -   Hai Nguyen, Google
--
--     -   Jean-François Roy, Google
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Piers Daniell, Nvidia
--
--     -   James Fitzpatrick, Imagination
--
--     -   Piotr Byszewski, Mobica
--
--     -   Jesse Hall, Google
--
--     -   Mike Blumenkrantz, Valve
--
-- == Description
--
-- This extension allows applications to create single-pass render pass
-- instances without needing to create render pass objects or framebuffers.
-- Dynamic render passes can also span across multiple primary command
-- buffers, rather than relying on secondary command buffers.
--
-- This extension also incorporates
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_NONE_KHR'
-- from <VK_QCOM_render_pass_store_ops.html VK_QCOM_render_pass_store_ops>,
-- enabling applications to avoid unnecessary synchronization when an
-- attachment is not written during a render pass.
--
-- == New Commands
--
-- -   'cmdBeginRenderingKHR'
--
-- -   'cmdEndRenderingKHR'
--
-- == New Structures
--
-- -   'RenderingAttachmentInfoKHR'
--
-- -   'RenderingInfoKHR'
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'CommandBufferInheritanceRenderingInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineRenderingCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDynamicRenderingFeaturesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples VK_AMD_mixed_attachment_samples>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'AttachmentSampleCountInfoAMD'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending 'RenderingInfoKHR':
--
--     -   'RenderingFragmentDensityMapAttachmentInfoEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>
-- is supported:
--
-- -   Extending 'RenderingInfoKHR':
--
--     -   'RenderingFragmentShadingRateAttachmentInfoKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'AttachmentSampleCountInfoNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_multiview_per_view_attributes VK_NVX_multiview_per_view_attributes>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'RenderingInfoKHR':
--
--     -   'MultiviewPerViewAttributesInfoNVX'
--
-- == New Enums
--
-- -   'RenderingFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'RenderingFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DYNAMIC_RENDERING_EXTENSION_NAME'
--
-- -   'KHR_DYNAMIC_RENDERING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp':
--
--     -   'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_NONE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples VK_AMD_mixed_attachment_samples>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_multiview_per_view_attributes VK_NVX_multiview_per_view_attributes>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX'
--
-- == Version History
--
-- -   Revision 1, 2021-10-06 (Tobias Hector)
--
--     -   Initial revision
--
-- == See Also
--
-- 'CommandBufferInheritanceRenderingInfoKHR',
-- 'PhysicalDeviceDynamicRenderingFeaturesKHR',
-- 'PipelineRenderingCreateInfoKHR', 'RenderingAttachmentInfoKHR',
-- 'RenderingFlagBitsKHR', 'RenderingFlagsKHR', 'RenderingInfoKHR',
-- 'cmdBeginRenderingKHR', 'cmdEndRenderingKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_dynamic_rendering  ( cmdBeginRenderingKHR
                                                   , cmdUseRenderingKHR
                                                   , cmdEndRenderingKHR
                                                   , pattern STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_NV
                                                   , PipelineRenderingCreateInfoKHR(..)
                                                   , RenderingInfoKHR(..)
                                                   , RenderingAttachmentInfoKHR(..)
                                                   , RenderingFragmentShadingRateAttachmentInfoKHR(..)
                                                   , RenderingFragmentDensityMapAttachmentInfoEXT(..)
                                                   , PhysicalDeviceDynamicRenderingFeaturesKHR(..)
                                                   , CommandBufferInheritanceRenderingInfoKHR(..)
                                                   , AttachmentSampleCountInfoAMD(..)
                                                   , MultiviewPerViewAttributesInfoNVX(..)
                                                   , RenderingFlagsKHR
                                                   , RenderingFlagBitsKHR( RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR
                                                                         , RENDERING_SUSPENDING_BIT_KHR
                                                                         , RENDERING_RESUMING_BIT_KHR
                                                                         , ..
                                                                         )
                                                   , AttachmentSampleCountInfoNV
                                                   , KHR_DYNAMIC_RENDERING_SPEC_VERSION
                                                   , pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION
                                                   , KHR_DYNAMIC_RENDERING_EXTENSION_NAME
                                                   , pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME
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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Core10.Enums.AttachmentLoadOp (AttachmentLoadOp)
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.CommandBufferBuilding (ClearValue)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginRenderingKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndRenderingKHR))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_INFO_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderingKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct RenderingInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct RenderingInfoKHR) -> IO ()

-- | vkCmdBeginRenderingKHR - Begin a dynamic render pass instance
--
-- = Description
--
-- After beginning a render pass instance, the command buffer is ready to
-- record
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing draw commands>.
--
-- If @pRenderingInfo->flags@ includes 'RENDERING_RESUMING_BIT_KHR' then
-- this render pass is resumed from a render pass instance that has been
-- suspended earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginRenderingKHR-dynamicRendering-06446# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRendering dynamicRendering>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBeginRenderingKHR-commandBuffer-06068# If @commandBuffer@
--     is a secondary command buffer, @pRenderingInfo->flags@ /must/ not
--     include 'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginRenderingKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginRenderingKHR-pRenderingInfo-parameter#
--     @pRenderingInfo@ /must/ be a valid pointer to a valid
--     'RenderingInfoKHR' structure
--
-- -   #VUID-vkCmdBeginRenderingKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginRenderingKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginRenderingKHR-renderpass# This command /must/ only be
--     called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'RenderingInfoKHR'
cmdBeginRenderingKHR :: forall a io
                      . (Extendss RenderingInfoKHR a, PokeChain a, MonadIO io)
                     => -- | @commandBuffer@ is the command buffer in which to record the command.
                        CommandBuffer
                     -> -- | @pRenderingInfo@ is a pointer to a 'RenderingInfoKHR' structure
                        -- specifying details of the render pass instance to begin.
                        (RenderingInfoKHR a)
                     -> io ()
cmdBeginRenderingKHR commandBuffer renderingInfo = liftIO . evalContT $ do
  let vkCmdBeginRenderingKHRPtr = pVkCmdBeginRenderingKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginRenderingKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginRenderingKHR is null" Nothing Nothing
  let vkCmdBeginRenderingKHR' = mkVkCmdBeginRenderingKHR vkCmdBeginRenderingKHRPtr
  pRenderingInfo <- ContT $ withCStruct (renderingInfo)
  lift $ traceAroundEvent "vkCmdBeginRenderingKHR" (vkCmdBeginRenderingKHR' (commandBufferHandle (commandBuffer)) (forgetExtensions pRenderingInfo))
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginRenderingKHR' and 'cmdEndRenderingKHR'
--
-- Note that 'cmdEndRenderingKHR' is *not* called if an exception is thrown
-- by the inner action.
cmdUseRenderingKHR :: forall a io r . (Extendss RenderingInfoKHR a, PokeChain a, MonadIO io) => CommandBuffer -> RenderingInfoKHR a -> io r -> io r
cmdUseRenderingKHR commandBuffer pRenderingInfo a =
  (cmdBeginRenderingKHR commandBuffer pRenderingInfo) *> a <* (cmdEndRenderingKHR commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderingKHR
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndRenderingKHR - End a dynamic render pass instance
--
-- = Description
--
-- If the value of @pRenderingInfo->flags@ used to begin this render pass
-- instance included 'RENDERING_SUSPENDING_BIT_KHR', then this render pass
-- is suspended and will be resumed later in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndRenderingKHR-None-06161# The current render pass
--     instance /must/ have been begun with 'cmdBeginRenderingKHR'
--
-- -   #VUID-vkCmdEndRenderingKHR-commandBuffer-06162# The current render
--     pass instance /must/ have been begun in @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndRenderingKHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndRenderingKHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndRenderingKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdEndRenderingKHR-renderpass# This command /must/ only be
--     called inside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndRenderingKHR :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer in which to record the command.
                      CommandBuffer
                   -> io ()
cmdEndRenderingKHR commandBuffer = liftIO $ do
  let vkCmdEndRenderingKHRPtr = pVkCmdEndRenderingKHR (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndRenderingKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndRenderingKHR is null" Nothing Nothing
  let vkCmdEndRenderingKHR' = mkVkCmdEndRenderingKHR vkCmdEndRenderingKHRPtr
  traceAroundEvent "vkCmdEndRenderingKHR" (vkCmdEndRenderingKHR' (commandBufferHandle (commandBuffer)))
  pure $ ()


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_NV"
pattern STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_NV = STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD


-- | VkPipelineRenderingCreateInfoKHR - Structure specifying attachment
-- formats
--
-- = Description
--
-- When a pipeline is created without a 'Vulkan.Core10.Handles.RenderPass',
-- if this structure is present in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', it specifies the
-- view mask and format of attachments used for rendering. If this
-- structure is not specified, and the pipeline does not include a
-- 'Vulkan.Core10.Handles.RenderPass', @viewMask@ and
-- @colorAttachmentCount@ are @0@, and @depthAttachmentFormat@ and
-- @stencilAttachmentFormat@ are
-- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'. If a graphics pipeline is
-- created with a valid 'Vulkan.Core10.Handles.RenderPass', parameters of
-- this structure are ignored.
--
-- If @depthAttachmentFormat@, @stencilAttachmentFormat@, or any element of
-- @pColorAttachmentFormats@ is
-- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it indicates that the
-- corresponding attachment is unused within the render pass. Valid formats
-- indicate that an attachment /can/ be used - but it is still valid to set
-- the attachment to @NULL@ when beginning rendering.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-pColorAttachmentFormats-06064#
--     If any element of @pColorAttachmentFormats@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-depthAttachmentFormat-06065#
--     If @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-stencilAttachmentFormat-06164#
--     If @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-depthAttachmentFormat-06165#
--     If @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' and
--     @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     @depthAttachmentFormat@ /must/ equal @stencilAttachmentFormat@
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-multiview-06066# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiview multiview>
--     feature is not enabled, @viewMask@ /must/ be @0@
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-viewMask-06067# The index of
--     the most significant bit in @viewMask@ /must/ be less than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxMultiviewViewCount maxMultiviewViewCount>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR'
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-pColorAttachmentFormats-parameter#
--     If @colorAttachmentCount@ is not @0@, @pColorAttachmentFormats@
--     /must/ be a valid pointer to an array of @colorAttachmentCount@
--     valid 'Vulkan.Core10.Enums.Format.Format' values
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-depthAttachmentFormat-parameter#
--     @depthAttachmentFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkPipelineRenderingCreateInfoKHR-stencilAttachmentFormat-parameter#
--     @stencilAttachmentFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRenderingCreateInfoKHR = PipelineRenderingCreateInfoKHR
  { -- | @viewMask@ is the viewMask used for rendering.
    viewMask :: Word32
  , -- | @pColorAttachmentFormats@ is an array of
    -- 'Vulkan.Core10.Enums.Format.Format' values defining the format of color
    -- attachments used in this pipeline.
    colorAttachmentFormats :: Vector Format
  , -- | @depthAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the depth attachment used in this pipeline.
    depthAttachmentFormat :: Format
  , -- | @stencilAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the stencil attachment used in this pipeline.
    stencilAttachmentFormat :: Format
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRenderingCreateInfoKHR)
#endif
deriving instance Show PipelineRenderingCreateInfoKHR

instance ToCStruct PipelineRenderingCreateInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRenderingCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentFormats)) :: Word32))
    pPColorAttachmentFormats' <- ContT $ allocaBytes @Format ((Data.Vector.length (colorAttachmentFormats)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (colorAttachmentFormats)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Format))) (pPColorAttachmentFormats')
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (depthAttachmentFormat)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (stencilAttachmentFormat)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    f

instance FromCStruct PipelineRenderingCreateInfoKHR where
  peekCStruct p = do
    viewMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pColorAttachmentFormats <- peek @(Ptr Format) ((p `plusPtr` 24 :: Ptr (Ptr Format)))
    pColorAttachmentFormats' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Format ((pColorAttachmentFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    depthAttachmentFormat <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    stencilAttachmentFormat <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    pure $ PipelineRenderingCreateInfoKHR
             viewMask pColorAttachmentFormats' depthAttachmentFormat stencilAttachmentFormat

instance Zero PipelineRenderingCreateInfoKHR where
  zero = PipelineRenderingCreateInfoKHR
           zero
           mempty
           zero
           zero


-- | VkRenderingInfoKHR - Structure specifying render pass instance begin
-- info
--
-- = Description
--
-- If @viewMask@ is not @0@, multiview is enabled.
--
-- If there is an instance of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
-- included in the @pNext@ chain and its @deviceCount@ member is not @0@,
-- then @renderArea@ is ignored, and the render area is defined per-device
-- by that structure.
--
-- Each element of the @pColorAttachments@ array corresponds to an output
-- location in the shader, i.e. if the shader declares an output variable
-- decorated with a @Location@ value of __X__, then it uses the attachment
-- provided in @pColorAttachments@[__X__]. If the @imageView@ member of any
-- element of @pColorAttachments@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', writes to the corresponding
-- location by a fragment are discarded.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingInfoKHR-viewMask-06069# If @viewMask@ is @0@,
--     @layerCount@ /must/ not be @0@
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06070# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples VK_AMD_mixed_attachment_samples>
--     nor the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>
--     extensions are enabled, @imageView@ members of @pDepthAttachment@,
--     @pStencilAttachment@, and elements of @pColorAttachments@ that are
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been
--     created with the same @sampleCount@
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06077# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.x@ /must/ be greater than or equal to 0
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06078# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.y@ /must/ be greater than or equal to 0
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06079# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0, the width of
--     the @imageView@ member of any element of @pColorAttachments@,
--     @pDepthAttachment@, or @pStencilAttachment@ that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be greater than or
--     equal to @renderArea.offset.x@ + @renderArea.extent.width@
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06080# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0, the height of
--     the @imageView@ member of any element of @pColorAttachments@,
--     @pDepthAttachment@, or @pStencilAttachment@ that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be greater than or
--     equal to @renderArea.offset.y@ + @renderArea.extent.height@
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06081# If the @pNext@ chain contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the @offset.x@ member of each element of @pDeviceRenderAreas@ /must/
--     be greater than or equal to 0
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06082# If the @pNext@ chain contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the @offset.y@ member of each element of @pDeviceRenderAreas@ /must/
--     be greater than or equal to 0
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06083# If the @pNext@ chain contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the width of the @imageView@ member of any element of
--     @pColorAttachments@, @pDepthAttachment@, or @pStencilAttachment@
--     that is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be
--     greater than or equal to the sum of the @offset.x@ and
--     @extent.width@ members of each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06084# If the @pNext@ chain contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the height of the @imageView@ member of any element of
--     @pColorAttachments@, @pDepthAttachment@, or @pStencilAttachment@
--     that is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be
--     greater than or equal to the sum of the @offset.y@ and
--     @extent.height@ members of each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06085# If neither
--     @pDepthAttachment@ or @pStencilAttachment@ are @NULL@ and the
--     @imageView@ member of either structure is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @imageView@ member of
--     each structure /must/ be the same
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06086# If neither
--     @pDepthAttachment@ or @pStencilAttachment@ are @NULL@, and the
--     @resolveMode@ member of each is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', the
--     @resolveImageView@ member of each structure /must/ be the same
--
-- -   #VUID-VkRenderingInfoKHR-colorAttachmentCount-06087# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', that @imageView@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06088# If
--     @pDepthAttachment@ is not @NULL@ and @pDepthAttachment->imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pDepthAttachment->imageView@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkRenderingInfoKHR-pStencilAttachment-06089# If
--     @pStencilAttachment@ is not @NULL@ and
--     @pStencilAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pStencilAttachment->imageView@ /must/ have been created with a
--     stencil usage including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkRenderingInfoKHR-colorAttachmentCount-06090# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @layout@ member of
--     that element of @pColorAttachments@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-colorAttachmentCount-06091# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', if the @resolveMode@
--     member of that element of @pColorAttachments@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', its
--     @resolveImageLayout@ member /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06092# If
--     @pDepthAttachment@ is not @NULL@ and @pDepthAttachment->imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pDepthAttachment->layout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06093# If
--     @pDepthAttachment@ is not @NULL@, @pDepthAttachment->imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pDepthAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pDepthAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-pStencilAttachment-06094# If
--     @pStencilAttachment@ is not @NULL@ and
--     @pStencilAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pStencilAttachment->layout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-pStencilAttachment-06095# If
--     @pStencilAttachment@ is not @NULL@, @pStencilAttachment->imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pStencilAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pStencilAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-colorAttachmentCount-06096# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @layout@ member of
--     that element of @pColorAttachments@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-colorAttachmentCount-06097# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', if the @resolveMode@
--     member of that element of @pColorAttachments@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', its
--     @resolveImageLayout@ member /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06098# If
--     @pDepthAttachment@ is not @NULL@, @pDepthAttachment->imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pDepthAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pDepthAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-pStencilAttachment-06099# If
--     @pStencilAttachment@ is not @NULL@, @pStencilAttachment->imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pStencilAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pStencilAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-colorAttachmentCount-06100# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @layout@ member of
--     that element of @pColorAttachments@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-colorAttachmentCount-06101# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', if the @resolveMode@
--     member of that element of @pColorAttachments@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', its
--     @resolveImageLayout@ member /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06102# If
--     @pDepthAttachment@ is not @NULL@ and @pDepthAttachment->imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pDepthAttachment->resolveMode@ /must/ be one of the bits set in
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties'::@supportedDepthResolveModes@
--
-- -   #VUID-VkRenderingInfoKHR-pStencilAttachment-06103# If
--     @pStencilAttachment@ is not @NULL@ and
--     @pStencilAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pStencilAttachment->resolveMode@ /must/ be one of the bits set in
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties'::@supportedStencilResolveModes@
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06104# If
--     @pDepthAttachment@ or @pStencilAttachment@ are both not @NULL@,
--     @pDepthAttachment->imageView@ and @pStencilAttachment->imageView@
--     are both not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties'::@independentResolveNone@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', the @resolveMode@ of both
--     structures /must/ be the same value
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-06105# If
--     @pDepthAttachment@ or @pStencilAttachment@ are both not @NULL@,
--     @pDepthAttachment->imageView@ and @pStencilAttachment->imageView@
--     are both not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties'::@independentResolve@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and the @resolveMode@ of
--     neither structure is
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', the
--     @resolveMode@ of both structures /must/ be the same value
--
-- -   #VUID-VkRenderingInfoKHR-colorAttachmentCount-06106#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxColorAttachments@
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06107# If the @imageView@ member
--     of a 'RenderingFragmentDensityMapAttachmentInfoEXT' structure
--     included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMapNonSubsampledImages non-subsample image feature>
--     is not enabled, valid @imageView@ and @resolveImageView@ members of
--     @pDepthAttachment@, @pStencilAttachment@, and each element of
--     @pColorAttachments@ /must/ be a 'Vulkan.Core10.Handles.ImageView'
--     created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06108# If the @imageView@ member
--     of a 'RenderingFragmentDensityMapAttachmentInfoEXT' structure
--     included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and @viewMask@ is not @0@,
--     @imageView@ /must/ have a @layerCount@ greater than or equal to the
--     index of the most significant bit in @viewMask@
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06109# If the @imageView@ member
--     of a 'RenderingFragmentDensityMapAttachmentInfoEXT' structure
--     included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and @viewMask@ is @0@,
--     @imageView@ /must/ have a @layerCount@ equal to @1@
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06112# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0 and the
--     @imageView@ member of a
--     'RenderingFragmentDensityMapAttachmentInfoEXT' structure included in
--     the @pNext@ chain is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ have a width greater than or equal to
--     \(\left\lceil{\frac{renderArea_{x}+renderArea_{width}}{maxFragmentDensityTexelSize_{width}}}\right\rceil\)
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06113# If the @pNext@ chain contains
--     a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     structure, its @deviceRenderAreaCount@ member is not 0, and the
--     @imageView@ member of a
--     'RenderingFragmentDensityMapAttachmentInfoEXT' structure included in
--     the @pNext@ chain is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ have a width greater than or equal to
--     \(\left\lceil{\frac{pDeviceRenderAreas_{x}+pDeviceRenderAreas_{width}}{maxFragmentDensityTexelSize_{width}}}\right\rceil\)
--     for each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06114# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0 and the
--     @imageView@ member of a
--     'RenderingFragmentDensityMapAttachmentInfoEXT' structure included in
--     the @pNext@ chain is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ have a height greater than or equal to
--     \(\left\lceil{\frac{renderArea_{y}+renderArea_{height}}{maxFragmentDensityTexelSize_{height}}}\right\rceil\)
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06115# If the @pNext@ chain contains
--     a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     structure, its @deviceRenderAreaCount@ member is not 0, and the
--     @imageView@ member of a
--     'RenderingFragmentDensityMapAttachmentInfoEXT' structure included in
--     the @pNext@ chain is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ have a height greater than or equal to
--     \(\left\lceil{\frac{pDeviceRenderAreas_{y}+pDeviceRenderAreas_{height}}{maxFragmentDensityTexelSize_{height}}}\right\rceil\)
--     for each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06116# If the @imageView@ member
--     of a 'RenderingFragmentDensityMapAttachmentInfoEXT' structure
--     included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ not be equal to
--     the @imageView@ or @resolveImageView@ member of @pDepthAttachment@,
--     @pStencilAttachment@, or any element of @pColorAttachments@
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06119# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0 and the
--     @imageView@ member of a
--     'RenderingFragmentShadingRateAttachmentInfoKHR' structure included
--     in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     width greater than or equal to
--     \(\left\lceil{\frac{renderArea_{x}+renderArea_{width}}{shadingRateAttachmentTexelSize_{width}}}\right\rceil\)
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06120# If the @pNext@ chain contains
--     a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     structure, its @deviceRenderAreaCount@ member is not 0, and the
--     @imageView@ member of a
--     'RenderingFragmentShadingRateAttachmentInfoKHR' structure included
--     in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     width greater than or equal to
--     \(\left\lceil{\frac{pDeviceRenderAreas_{x}+pDeviceRenderAreas_{width}}{shadingRateAttachmentTexelSize_{width}}}\right\rceil\)
--     for each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06121# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0 and the
--     @imageView@ member of a
--     'RenderingFragmentShadingRateAttachmentInfoKHR' structure included
--     in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     height greater than or equal to
--     \(\left\lceil{\frac{renderArea_{y}+renderArea_{height}}{shadingRateAttachmentTexelSize_{height}}}\right\rceil\)
--
-- -   #VUID-VkRenderingInfoKHR-pNext-06122# If the @pNext@ chain contains
--     a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     structure, its @deviceRenderAreaCount@ member is not 0, and the
--     @imageView@ member of a
--     'RenderingFragmentShadingRateAttachmentInfoKHR' structure included
--     in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     height greater than or equal to
--     \(\left\lceil{\frac{pDeviceRenderAreas_{y}+pDeviceRenderAreas_{height}}{shadingRateAttachmentTexelSize_{height}}}\right\rceil\)
--     for each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06123# If the @imageView@ member
--     of a 'RenderingFragmentShadingRateAttachmentInfoKHR' structure
--     included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and @viewMask@ is @0@,
--     @imageView@ /must/ have a @layerCount@ that is either equal to @1@
--     or greater than or equal to @layerCount@
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06124# If the @imageView@ member
--     of a 'RenderingFragmentShadingRateAttachmentInfoKHR' structure
--     included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and @viewMask@ is not @0@,
--     @imageView@ /must/ have a @layerCount@ that either equal to @1@ or
--     greater than or equal to the index of the most significant bit in
--     @viewMask@
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06125# If the @imageView@ member
--     of a 'RenderingFragmentShadingRateAttachmentInfoKHR' structure
--     included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ not be equal to
--     the @imageView@ or @resolveImageView@ member of @pDepthAttachment@,
--     @pStencilAttachment@, or any element of @pColorAttachments@
--
-- -   #VUID-VkRenderingInfoKHR-imageView-06126# If the @imageView@ member
--     of a 'RenderingFragmentShadingRateAttachmentInfoKHR' structure
--     included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ not be equal to
--     the @imageView@ member of a
--     'RenderingFragmentDensityMapAttachmentInfoEXT' structure included in
--     the @pNext@ chain
--
-- -   #VUID-VkRenderingInfoKHR-multiview-06127# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiview multiview>
--     feature is not enabled, @viewMask@ /must/ be @0@
--
-- -   #VUID-VkRenderingInfoKHR-viewMask-06128# The index of the most
--     significant bit in @viewMask@ /must/ be less than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxMultiviewViewCount maxMultiviewViewCount>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_INFO_KHR'
--
-- -   #VUID-VkRenderingInfoKHR-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     'MultiviewPerViewAttributesInfoNVX',
--     'RenderingFragmentDensityMapAttachmentInfoEXT', or
--     'RenderingFragmentShadingRateAttachmentInfoKHR'
--
-- -   #VUID-VkRenderingInfoKHR-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkRenderingInfoKHR-flags-parameter# @flags@ /must/ be a valid
--     combination of 'RenderingFlagBitsKHR' values
--
-- -   #VUID-VkRenderingInfoKHR-pColorAttachments-parameter# If
--     @colorAttachmentCount@ is not @0@, @pColorAttachments@ /must/ be a
--     valid pointer to an array of @colorAttachmentCount@ valid
--     'RenderingAttachmentInfoKHR' structures
--
-- -   #VUID-VkRenderingInfoKHR-pDepthAttachment-parameter# If
--     @pDepthAttachment@ is not @NULL@, @pDepthAttachment@ /must/ be a
--     valid pointer to a valid 'RenderingAttachmentInfoKHR' structure
--
-- -   #VUID-VkRenderingInfoKHR-pStencilAttachment-parameter# If
--     @pStencilAttachment@ is not @NULL@, @pStencilAttachment@ /must/ be a
--     valid pointer to a valid 'RenderingAttachmentInfoKHR' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.FundamentalTypes.Rect2D', 'RenderingAttachmentInfoKHR',
-- 'RenderingFlagsKHR', 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBeginRenderingKHR'
data RenderingInfoKHR (es :: [Type]) = RenderingInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of 'RenderingFlagBitsKHR'.
    flags :: RenderingFlagsKHR
  , -- | @renderArea@ is the render area that is affected by the render pass
    -- instance.
    renderArea :: Rect2D
  , -- | @layerCount@ is the number of layers rendered to in each attachment when
    -- @viewMask@ is @0@.
    layerCount :: Word32
  , -- | @viewMask@ is the view mask indicating the indices of attachment layers
    -- that will be rendered when it is not @0@.
    viewMask :: Word32
  , -- | @pColorAttachments@ is a pointer to an array of @colorAttachmentCount@
    -- 'RenderingAttachmentInfoKHR' structures describing any color attachments
    -- used.
    colorAttachments :: Vector RenderingAttachmentInfoKHR
  , -- | @pDepthAttachment@ is a pointer to a 'RenderingAttachmentInfoKHR'
    -- structure describing a depth attachment.
    depthAttachment :: Maybe RenderingAttachmentInfoKHR
  , -- | @pStencilAttachment@ is a pointer to a 'RenderingAttachmentInfoKHR'
    -- structure describing a stencil attachment.
    stencilAttachment :: Maybe RenderingAttachmentInfoKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderingInfoKHR es)

instance Extensible RenderingInfoKHR where
  extensibleTypeName = "RenderingInfoKHR"
  setNext x next = x{next = next}
  getNext RenderingInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderingInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MultiviewPerViewAttributesInfoNVX = Just f
    | Just Refl <- eqT @e @RenderingFragmentDensityMapAttachmentInfoEXT = Just f
    | Just Refl <- eqT @e @RenderingFragmentShadingRateAttachmentInfoKHR = Just f
    | Just Refl <- eqT @e @DeviceGroupRenderPassBeginInfo = Just f
    | otherwise = Nothing

instance (Extendss RenderingInfoKHR es, PokeChain es) => ToCStruct (RenderingInfoKHR es) where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderingFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Rect2D)) (renderArea)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (layerCount)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachments)) :: Word32))
    pPColorAttachments' <- ContT $ allocaBytes @RenderingAttachmentInfoKHR ((Data.Vector.length (colorAttachments)) * 72)
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPColorAttachments' `plusPtr` (72 * (i)) :: Ptr RenderingAttachmentInfoKHR) (e) . ($ ())) (colorAttachments)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr RenderingAttachmentInfoKHR))) (pPColorAttachments')
    pDepthAttachment'' <- case (depthAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr RenderingAttachmentInfoKHR))) pDepthAttachment''
    pStencilAttachment'' <- case (stencilAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr RenderingAttachmentInfoKHR))) pStencilAttachment''
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Rect2D)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ f

instance es ~ '[] => Zero (RenderingInfoKHR es) where
  zero = RenderingInfoKHR
           ()
           zero
           zero
           zero
           zero
           mempty
           Nothing
           Nothing


-- | VkRenderingAttachmentInfoKHR - Structure specifying attachment
-- information
--
-- = Description
--
-- Values in @imageView@ are loaded and stored according to the values of
-- @loadOp@ and @storeOp@, within the render area for each device specified
-- in 'RenderingInfoKHR'. If @imageView@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', other members of this
-- structure are ignored; writes to this attachment will be discarded, and
-- no load, store, or resolve operations will be performed.
--
-- If @resolveMode@ is
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', then
-- @resolveImageView@ is ignored. If @resolveMode@ is not
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', values in
-- @resolveImageView@ within the render area become undefined once
-- rendering begins. At the end of rendering, the color values written to
-- each pixel location in @imageView@ will be resolved according to
-- @resolveMode@ and stored into the the same location in
-- @resolveImageView@.
--
-- Note
--
-- The resolve mode and store operation are independent; it is valid to
-- write both resolved and unresolved values, and equally valid to discard
-- the unresolved values while writing the resolved ones.
--
-- Store and resolve operations are only performed at the end of a render
-- pass instance that does not specify the 'RENDERING_SUSPENDING_BIT_KHR'
-- flag.
--
-- Load operations are only performed at the beginning of a render pass
-- instance that does not specify the 'RENDERING_RESUMING_BIT_KHR' flag.
--
-- Image contents at the end of a suspended render pass instance remain
-- defined for access by a resuming render pass instance.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06129# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and has a
--     non-integer color format, @resolveMode@ /must/ be
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE' or
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_AVERAGE_BIT'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06130# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and has an integer
--     color format, @resolveMode@ /must/ be
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE' or
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06132# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @imageView@ /must/ not have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06133# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageView@ /must/ have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06134# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @imageView@ and @resolveImageView@ /must/ have the same
--     'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06135# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ not
--     be 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06136# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06137# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06138# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ not
--     be
--     'Vulkan.Extensions.VK_NV_shading_rate_image.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06139# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Extensions.VK_NV_shading_rate_image.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06140# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ not
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06141# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06142# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06143# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ not
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06144# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06145# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ not
--     be 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-06146# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageView-parameter# If
--     @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView'
--     handle
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-imageLayout-parameter#
--     @imageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-resolveMode-parameter# If
--     @resolveMode@ is not @0@, @resolveMode@ /must/ be a valid
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-resolveImageView-parameter# If
--     @resolveImageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @resolveImageView@ /must/ be a valid
--     'Vulkan.Core10.Handles.ImageView' handle
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-resolveImageLayout-parameter#
--     @resolveImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-loadOp-parameter# @loadOp@ /must/
--     be a valid 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp'
--     value
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-storeOp-parameter# @storeOp@
--     /must/ be a valid
--     'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp' value
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-clearValue-parameter#
--     @clearValue@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ClearValue' union
--
-- -   #VUID-VkRenderingAttachmentInfoKHR-commonparent# Both of
--     @imageView@, and @resolveImageView@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp',
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp',
-- 'Vulkan.Core10.CommandBufferBuilding.ClearValue',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView', 'RenderingInfoKHR',
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderingAttachmentInfoKHR = RenderingAttachmentInfoKHR
  { -- | @imageView@ is the image view that will be used for rendering.
    imageView :: ImageView
  , -- | @imageLayout@ is the layout that @imageView@ will be in during
    -- rendering.
    imageLayout :: ImageLayout
  , -- | @resolveMode@ is a
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
    -- defining how multisampled data written to @imageView@ will be resolved.
    resolveMode :: ResolveModeFlagBits
  , -- | @resolveImageView@ is an image view used to write resolved multisample
    -- data at the end of rendering.
    resolveImageView :: ImageView
  , -- | @resolveImageLayout@ is the layout that @resolveImageView@ will be in
    -- during rendering.
    resolveImageLayout :: ImageLayout
  , -- | @loadOp@ is a 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp'
    -- value specifying how the contents of @imageView@ are treated at the
    -- start of the render pass instance.
    loadOp :: AttachmentLoadOp
  , -- | @storeOp@ is a 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
    -- value specifying how the contents of @imageView@ are treated at the end
    -- of the render pass instance.
    storeOp :: AttachmentStoreOp
  , -- | @clearValue@ is a 'Vulkan.Core10.CommandBufferBuilding.ClearValue'
    -- structure that defines values used to clear @imageView@ when @loadOp@ is
    -- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'.
    clearValue :: ClearValue
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingAttachmentInfoKHR)
#endif
deriving instance Show RenderingAttachmentInfoKHR

instance ToCStruct RenderingAttachmentInfoKHR where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingAttachmentInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageView)) (imageView)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (imageLayout)
    lift $ poke ((p `plusPtr` 28 :: Ptr ResolveModeFlagBits)) (resolveMode)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageView)) (resolveImageView)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (resolveImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr AttachmentLoadOp)) (loadOp)
    lift $ poke ((p `plusPtr` 48 :: Ptr AttachmentStoreOp)) (storeOp)
    ContT $ pokeCStruct ((p `plusPtr` 52 :: Ptr ClearValue)) (clearValue) . ($ ())
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr AttachmentLoadOp)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr AttachmentStoreOp)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 52 :: Ptr ClearValue)) (zero) . ($ ())
    lift $ f

instance Zero RenderingAttachmentInfoKHR where
  zero = RenderingAttachmentInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkRenderingFragmentShadingRateAttachmentInfoKHR - Structure specifying
-- fragment shading rate attachment information
--
-- = Description
--
-- This structure can be included in the @pNext@ chain of
-- 'RenderingInfoKHR' to define a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>.
-- If @imageView@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', or if this
-- structure is not specified, the implementation behaves as if a valid
-- shading rate attachment was specified with all texels specifying a
-- single pixel per fragment.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06147#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @layout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06148#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it
--     /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06149#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.width@ /must/ be a power of two
--     value
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06150#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.width@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSize.width>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06151#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.width@ /must/ be greater than or
--     equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minFragmentShadingRateAttachmentTexelSize minFragmentShadingRateAttachmentTexelSize.width>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06152#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.height@ /must/ be a power of two
--     value
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06153#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.height@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSize.height>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06154#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.height@ /must/ be greater than or
--     equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minFragmentShadingRateAttachmentTexelSize minFragmentShadingRateAttachmentTexelSize.height>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06155#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     quotient of @shadingRateAttachmentTexelSize.width@ and
--     @shadingRateAttachmentTexelSize.height@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSizeAspectRatio maxFragmentShadingRateAttachmentTexelSizeAspectRatio>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06156#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     quotient of @shadingRateAttachmentTexelSize.height@ and
--     @shadingRateAttachmentTexelSize.width@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSizeAspectRatio maxFragmentShadingRateAttachmentTexelSizeAspectRatio>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR'
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-parameter#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView'
--     handle
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageLayout-parameter#
--     @imageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderingFragmentShadingRateAttachmentInfoKHR = RenderingFragmentShadingRateAttachmentInfoKHR
  { -- | @imageView@ is the image view that will be used as a fragment shading
    -- rate attachment.
    imageView :: ImageView
  , -- | @imageLayout@ is the layout that @imageView@ will be in during
    -- rendering.
    imageLayout :: ImageLayout
  , -- | @shadingRateAttachmentTexelSize@ specifies the number of pixels
    -- corresponding to each texel in @imageView@.
    shadingRateAttachmentTexelSize :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingFragmentShadingRateAttachmentInfoKHR)
#endif
deriving instance Show RenderingFragmentShadingRateAttachmentInfoKHR

instance ToCStruct RenderingFragmentShadingRateAttachmentInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingFragmentShadingRateAttachmentInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (imageView)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (imageLayout)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (shadingRateAttachmentTexelSize)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct RenderingFragmentShadingRateAttachmentInfoKHR where
  peekCStruct p = do
    imageView <- peek @ImageView ((p `plusPtr` 16 :: Ptr ImageView))
    imageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    shadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 28 :: Ptr Extent2D))
    pure $ RenderingFragmentShadingRateAttachmentInfoKHR
             imageView imageLayout shadingRateAttachmentTexelSize

instance Storable RenderingFragmentShadingRateAttachmentInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderingFragmentShadingRateAttachmentInfoKHR where
  zero = RenderingFragmentShadingRateAttachmentInfoKHR
           zero
           zero
           zero


-- | VkRenderingFragmentDensityMapAttachmentInfoEXT - Structure specifying
-- fragment shading rate attachment information
--
-- = Description
--
-- This structure can be included in the @pNext@ chain of
-- 'RenderingInfoKHR' to define a fragment density map. If @imageView@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', or if this structure is not
-- specified, @imageView@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageView-06157#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @layout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT'
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageView-06158#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it
--     /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageView-06159#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it
--     /must/ not have been created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT'
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageView-parameter#
--     @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView'
--     handle
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageLayout-parameter#
--     @imageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderingFragmentDensityMapAttachmentInfoEXT = RenderingFragmentDensityMapAttachmentInfoEXT
  { -- | @imageView@ is the image view that will be used as a fragment shading
    -- rate attachment.
    imageView :: ImageView
  , -- | @imageLayout@ is the layout that @imageView@ will be in during
    -- rendering.
    imageLayout :: ImageLayout
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingFragmentDensityMapAttachmentInfoEXT)
#endif
deriving instance Show RenderingFragmentDensityMapAttachmentInfoEXT

instance ToCStruct RenderingFragmentDensityMapAttachmentInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingFragmentDensityMapAttachmentInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (imageView)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (imageLayout)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct RenderingFragmentDensityMapAttachmentInfoEXT where
  peekCStruct p = do
    imageView <- peek @ImageView ((p `plusPtr` 16 :: Ptr ImageView))
    imageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    pure $ RenderingFragmentDensityMapAttachmentInfoEXT
             imageView imageLayout

instance Storable RenderingFragmentDensityMapAttachmentInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderingFragmentDensityMapAttachmentInfoEXT where
  zero = RenderingFragmentDensityMapAttachmentInfoEXT
           zero
           zero


-- | VkPhysicalDeviceDynamicRenderingFeaturesKHR - Structure indicating
-- support for dynamic render pass instances
--
-- = Members
--
-- The members of the 'PhysicalDeviceDynamicRenderingFeaturesKHR' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDynamicRenderingFeaturesKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDynamicRenderingFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDynamicRenderingFeaturesKHR = PhysicalDeviceDynamicRenderingFeaturesKHR
  { -- | #features-dynamicRendering# @dynamicRendering@ specifies that the
    -- implementation supports dynamic render pass instances using the
    -- 'cmdBeginRenderingKHR' command.
    dynamicRendering :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDynamicRenderingFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceDynamicRenderingFeaturesKHR

instance ToCStruct PhysicalDeviceDynamicRenderingFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDynamicRenderingFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dynamicRendering))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDynamicRenderingFeaturesKHR where
  peekCStruct p = do
    dynamicRendering <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDynamicRenderingFeaturesKHR
             (bool32ToBool dynamicRendering)

instance Storable PhysicalDeviceDynamicRenderingFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDynamicRenderingFeaturesKHR where
  zero = PhysicalDeviceDynamicRenderingFeaturesKHR
           zero


-- | VkCommandBufferInheritanceRenderingInfoKHR - Structure specifying
-- command buffer inheritance info for dynamic render pass instances
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes a
-- 'CommandBufferInheritanceRenderingInfoKHR' structure, then that
-- structure controls parameters of dynamic render pass instances that the
-- 'Vulkan.Core10.Handles.CommandBuffer' /can/ be executed within. If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@renderPass@
-- is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', or
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is not specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@flags@,
-- parameters of this structure are ignored.
--
-- If @colorAttachmentCount@ is @0@ and the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-variableMultisampleRate variableMultisampleRate>
-- feature is enabled, @rasterizationSamples@ is ignored.
--
-- If @depthAttachmentFormat@, @stencilAttachmentFormat@, or any element of
-- @pColorAttachmentFormats@ is
-- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it indicates that the
-- corresponding attachment is unused within the render pass.
--
-- == Valid Usage
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-colorAttachmentCount-06004#
--     If @colorAttachmentCount@ is not @0@, @rasterizationSamples@ /must/
--     be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-variableMultisampleRate-06005#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-variableMultisampleRate variableMultisampleRate>
--     feature is not enabled, @rasterizationSamples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-pColorAttachmentFormats-06006#
--     If any element of @pColorAttachmentFormats@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-depthAttachmentFormat-06007#
--     If @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-stencilAttachmentFormat-06199#
--     If @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-depthAttachmentFormat-06200#
--     If @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' and
--     @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     @depthAttachmentFormat@ /must/ equal @stencilAttachmentFormat@
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-multiview-06008# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiview multiview>
--     feature is not enabled, @viewMask@ /must/ be @0@
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-viewMask-06009# The
--     index of the most significant bit in @viewMask@ /must/ be less than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxMultiviewViewCount maxMultiviewViewCount>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR'
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-flags-parameter#
--     @flags@ /must/ be a valid combination of 'RenderingFlagBitsKHR'
--     values
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-pColorAttachmentFormats-parameter#
--     @pColorAttachmentFormats@ /must/ be a valid pointer to an array of
--     @colorAttachmentCount@ valid 'Vulkan.Core10.Enums.Format.Format'
--     values
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-depthAttachmentFormat-parameter#
--     @depthAttachmentFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-stencilAttachmentFormat-parameter#
--     @stencilAttachmentFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-rasterizationSamples-parameter#
--     If @rasterizationSamples@ is not @0@, @rasterizationSamples@ /must/
--     be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-colorAttachmentCount-arraylength#
--     @colorAttachmentCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.Enums.Format.Format', 'RenderingFlagsKHR',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data CommandBufferInheritanceRenderingInfoKHR = CommandBufferInheritanceRenderingInfoKHR
  { -- | @flags@ is a bitmask of 'RenderingFlagBitsKHR' used by the render pass
    -- instance.
    flags :: RenderingFlagsKHR
  , -- | @viewMask@ is the view mask used for rendering.
    viewMask :: Word32
  , -- | @pColorAttachmentFormats@ is an array of
    -- 'Vulkan.Core10.Enums.Format.Format' values defining the format of color
    -- attachments.
    colorAttachmentFormats :: Vector Format
  , -- | @depthAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the depth attachment.
    depthAttachmentFormat :: Format
  , -- | @stencilAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the stencil attachment.
    stencilAttachmentFormat :: Format
  , -- | @rasterizationSamples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' specifying
    -- the number of samples used in rasterization.
    rasterizationSamples :: SampleCountFlagBits
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferInheritanceRenderingInfoKHR)
#endif
deriving instance Show CommandBufferInheritanceRenderingInfoKHR

instance ToCStruct CommandBufferInheritanceRenderingInfoKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceRenderingInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderingFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentFormats)) :: Word32))
    pPColorAttachmentFormats' <- ContT $ allocaBytes @Format ((Data.Vector.length (colorAttachmentFormats)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (colorAttachmentFormats)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Format))) (pPColorAttachmentFormats')
    lift $ poke ((p `plusPtr` 40 :: Ptr Format)) (depthAttachmentFormat)
    lift $ poke ((p `plusPtr` 44 :: Ptr Format)) (stencilAttachmentFormat)
    lift $ poke ((p `plusPtr` 48 :: Ptr SampleCountFlagBits)) (rasterizationSamples)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Format)) (zero)
    f

instance FromCStruct CommandBufferInheritanceRenderingInfoKHR where
  peekCStruct p = do
    flags <- peek @RenderingFlagsKHR ((p `plusPtr` 16 :: Ptr RenderingFlagsKHR))
    viewMask <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pColorAttachmentFormats <- peek @(Ptr Format) ((p `plusPtr` 32 :: Ptr (Ptr Format)))
    pColorAttachmentFormats' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Format ((pColorAttachmentFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    depthAttachmentFormat <- peek @Format ((p `plusPtr` 40 :: Ptr Format))
    stencilAttachmentFormat <- peek @Format ((p `plusPtr` 44 :: Ptr Format))
    rasterizationSamples <- peek @SampleCountFlagBits ((p `plusPtr` 48 :: Ptr SampleCountFlagBits))
    pure $ CommandBufferInheritanceRenderingInfoKHR
             flags viewMask pColorAttachmentFormats' depthAttachmentFormat stencilAttachmentFormat rasterizationSamples

instance Zero CommandBufferInheritanceRenderingInfoKHR where
  zero = CommandBufferInheritanceRenderingInfoKHR
           zero
           zero
           mempty
           zero
           zero
           zero


-- | VkAttachmentSampleCountInfoAMD - Structure specifying command buffer
-- inheritance info for dynamic render pass instances
--
-- = Description
--
-- If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@renderPass@
-- is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@flags@, and the
-- @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes
-- 'AttachmentSampleCountInfoAMD', then this structure defines the sample
-- counts of each attachment within the render pass instance. If
-- 'AttachmentSampleCountInfoAMD' is not included, the value of
-- 'CommandBufferInheritanceRenderingInfoKHR'::@rasterizationSamples@ is
-- used as the sample count for each attachment. If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@renderPass@
-- is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', or
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is not specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@flags@,
-- parameters of this structure are ignored.
--
-- 'AttachmentSampleCountInfoAMD' /can/ also be included in the @pNext@
-- chain of 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'. When a
-- graphics pipeline is created without a
-- 'Vulkan.Core10.Handles.RenderPass', if this structure is present in the
-- @pNext@ chain of 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', it
-- specifies the sample count of attachments used for rendering. If this
-- structure is not specified, and the pipeline does not include a
-- 'Vulkan.Core10.Handles.RenderPass', the value of
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
-- is used as the sample count for each attachment. If a graphics pipeline
-- is created with a valid 'Vulkan.Core10.Handles.RenderPass', parameters
-- of this structure are ignored.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAttachmentSampleCountInfoAMD-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD'
--
-- -   #VUID-VkAttachmentSampleCountInfoAMD-pColorAttachmentSamples-parameter#
--     @pColorAttachmentSamples@ /must/ be a valid pointer to an array of
--     @colorAttachmentCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' values
--
-- -   #VUID-VkAttachmentSampleCountInfoAMD-depthStencilAttachmentSamples-parameter#
--     If @depthStencilAttachmentSamples@ is not @0@,
--     @depthStencilAttachmentSamples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkAttachmentSampleCountInfoAMD-colorAttachmentCount-arraylength#
--     @colorAttachmentCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples VK_AMD_mixed_attachment_samples>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>,
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AttachmentSampleCountInfoAMD = AttachmentSampleCountInfoAMD
  { -- | @pColorAttachmentSamples@ is an array of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' values
    -- defining the sample count of color attachments.
    colorAttachmentSamples :: Vector SampleCountFlagBits
  , -- | @depthStencilAttachmentSamples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    -- defining the sample count of a depth\/stencil attachment.
    depthStencilAttachmentSamples :: SampleCountFlagBits
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentSampleCountInfoAMD)
#endif
deriving instance Show AttachmentSampleCountInfoAMD

instance ToCStruct AttachmentSampleCountInfoAMD where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentSampleCountInfoAMD{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentSamples)) :: Word32))
    pPColorAttachmentSamples' <- ContT $ allocaBytes @SampleCountFlagBits ((Data.Vector.length (colorAttachmentSamples)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentSamples' `plusPtr` (4 * (i)) :: Ptr SampleCountFlagBits) (e)) (colorAttachmentSamples)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr SampleCountFlagBits))) (pPColorAttachmentSamples')
    lift $ poke ((p `plusPtr` 32 :: Ptr SampleCountFlagBits)) (depthStencilAttachmentSamples)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct AttachmentSampleCountInfoAMD where
  peekCStruct p = do
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pColorAttachmentSamples <- peek @(Ptr SampleCountFlagBits) ((p `plusPtr` 24 :: Ptr (Ptr SampleCountFlagBits)))
    pColorAttachmentSamples' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @SampleCountFlagBits ((pColorAttachmentSamples `advancePtrBytes` (4 * (i)) :: Ptr SampleCountFlagBits)))
    depthStencilAttachmentSamples <- peek @SampleCountFlagBits ((p `plusPtr` 32 :: Ptr SampleCountFlagBits))
    pure $ AttachmentSampleCountInfoAMD
             pColorAttachmentSamples' depthStencilAttachmentSamples

instance Zero AttachmentSampleCountInfoAMD where
  zero = AttachmentSampleCountInfoAMD
           mempty
           zero


-- | VkMultiviewPerViewAttributesInfoNVX - Structure specifying the multiview
-- per-attribute properties
--
-- = Description
--
-- When dynamic render pass instances are being used, instead of specifying
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
-- or
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX'
-- in the subpass description flags, the per-attibute properties of the
-- render pass instance /must/ be specified by the
-- 'MultiviewPerViewAttributesInfoNVX' structure Include the
-- 'MultiviewPerViewAttributesInfoNVX' structure in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' when creating a
-- graphics pipeline for dynamic rendering, 'RenderingInfoKHR' when
-- starting a dynamic render pass instance, and
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' when
-- specifying the dynamic render pass instance parameters for secondary
-- command buffers.
--
-- == Valid Usage
--
-- -   #VUID-VkMultiviewPerViewAttributesInfoNVX-perViewAttributesPositionXOnly-06163#
--     If @perViewAttributesPositionXOnly@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE' then @perViewAttributes@
--     /must/ also be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMultiviewPerViewAttributesInfoNVX-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_multiview_per_view_attributes VK_NVX_multiview_per_view_attributes>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MultiviewPerViewAttributesInfoNVX = MultiviewPerViewAttributesInfoNVX
  { -- | @perViewAttributes@ specifies that shaders compiled for this pipeline
    -- write the attributes for all views in a single invocation of each vertex
    -- processing stage. All pipelines executed within a render pass instance
    -- that includes this bit /must/ write per-view attributes to the
    -- @*PerViewNV[]@ shader outputs, in addition to the non-per-view (e.g.
    -- @Position@) outputs.
    perViewAttributes :: Bool
  , -- | @perViewAttributesPositionXOnly@ specifies that shaders compiled for
    -- this pipeline use per-view positions which only differ in value in the x
    -- component. Per-view viewport mask /can/ also be used.
    perViewAttributesPositionXOnly :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MultiviewPerViewAttributesInfoNVX)
#endif
deriving instance Show MultiviewPerViewAttributesInfoNVX

instance ToCStruct MultiviewPerViewAttributesInfoNVX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MultiviewPerViewAttributesInfoNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (perViewAttributes))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (perViewAttributesPositionXOnly))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct MultiviewPerViewAttributesInfoNVX where
  peekCStruct p = do
    perViewAttributes <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    perViewAttributesPositionXOnly <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ MultiviewPerViewAttributesInfoNVX
             (bool32ToBool perViewAttributes) (bool32ToBool perViewAttributesPositionXOnly)

instance Storable MultiviewPerViewAttributesInfoNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MultiviewPerViewAttributesInfoNVX where
  zero = MultiviewPerViewAttributesInfoNVX
           zero
           zero


type RenderingFlagsKHR = RenderingFlagBitsKHR

-- | VkRenderingFlagBitsKHR - Bitmask specifying additional properties of a
-- dynamic render pass instance
--
-- = Description
--
-- The contents of @pRenderingInfo@ /must/ match between suspended render
-- pass instances and the render pass instances that resume them, other
-- than the presence or absence of the 'RENDERING_RESUMING_BIT_KHR',
-- 'RENDERING_SUSPENDING_BIT_KHR', and
-- 'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR' flags. No action
-- or synchronization commands, or other render pass instances, are allowed
-- between suspending and resuming render pass instances.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'RenderingFlagsKHR'
newtype RenderingFlagBitsKHR = RenderingFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR' specifies that
-- draw calls for the render pass instance will be recorded in secondary
-- command buffers.
pattern RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR = RenderingFlagBitsKHR 0x00000001
-- | 'RENDERING_SUSPENDING_BIT_KHR' specifies that the render pass instance
-- will be suspended.
pattern RENDERING_SUSPENDING_BIT_KHR                         = RenderingFlagBitsKHR 0x00000002
-- | 'RENDERING_RESUMING_BIT_KHR' specifies that the render pass instance is
-- resuming an earlier suspended render pass instance.
pattern RENDERING_RESUMING_BIT_KHR                           = RenderingFlagBitsKHR 0x00000004

conNameRenderingFlagBitsKHR :: String
conNameRenderingFlagBitsKHR = "RenderingFlagBitsKHR"

enumPrefixRenderingFlagBitsKHR :: String
enumPrefixRenderingFlagBitsKHR = "RENDERING_"

showTableRenderingFlagBitsKHR :: [(RenderingFlagBitsKHR, String)]
showTableRenderingFlagBitsKHR =
  [ (RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR, "CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR")
  , (RENDERING_SUSPENDING_BIT_KHR                        , "SUSPENDING_BIT_KHR")
  , (RENDERING_RESUMING_BIT_KHR                          , "RESUMING_BIT_KHR")
  ]

instance Show RenderingFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixRenderingFlagBitsKHR
                            showTableRenderingFlagBitsKHR
                            conNameRenderingFlagBitsKHR
                            (\(RenderingFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read RenderingFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixRenderingFlagBitsKHR
                          showTableRenderingFlagBitsKHR
                          conNameRenderingFlagBitsKHR
                          RenderingFlagBitsKHR


-- No documentation found for TopLevel "VkAttachmentSampleCountInfoNV"
type AttachmentSampleCountInfoNV = AttachmentSampleCountInfoAMD


type KHR_DYNAMIC_RENDERING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_SPEC_VERSION"
pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION = 1


type KHR_DYNAMIC_RENDERING_EXTENSION_NAME = "VK_KHR_dynamic_rendering"

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_EXTENSION_NAME"
pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME = "VK_KHR_dynamic_rendering"

