{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_create_renderpass2"
module Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2  ( createRenderPass2
                                                              , cmdBeginRenderPass2
                                                              , cmdUseRenderPass2
                                                              , cmdNextSubpass2
                                                              , cmdEndRenderPass2
                                                              , AttachmentDescription2(..)
                                                              , AttachmentReference2(..)
                                                              , SubpassDescription2(..)
                                                              , SubpassDependency2(..)
                                                              , RenderPassCreateInfo2(..)
                                                              , SubpassBeginInfo(..)
                                                              , SubpassEndInfo(..)
                                                              , StructureType(..)
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Enums.AttachmentDescriptionFlagBits (AttachmentDescriptionFlags)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (AttachmentDescriptionStencilLayout)
import Vulkan.Core10.Enums.AttachmentLoadOp (AttachmentLoadOp)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (AttachmentReferenceStencilLayout)
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginRenderPass2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndRenderPass2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdNextSubpass2))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRenderPass2))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import Vulkan.Core10.Enums.Format (Format)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateAttachmentInfoKHR)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (MemoryBarrier2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled (MultisampledRenderToSingleSampledInfoEXT)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (RenderPass)
import Vulkan.Core10.Handles (RenderPass(..))
import Vulkan.Core10.CommandBufferBuilding (RenderPassBeginInfo)
import Vulkan.Core10.Enums.RenderPassCreateFlagBits (RenderPassCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (RenderPassCreationControlEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (RenderPassCreationFeedbackCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (RenderPassFragmentDensityMapCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (RenderPassSubpassFeedbackCreateInfoEXT)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (SubpassDescriptionDepthStencilResolve)
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_fragment_density_map_offset (SubpassFragmentDensityMapOffsetEndInfoQCOM)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_END_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass2
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct RenderPassCreateInfo2) -> Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct RenderPassCreateInfo2) -> Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result

-- | vkCreateRenderPass2 - Create a new render pass object
--
-- = Description
--
-- This command is functionally identical to
-- 'Vulkan.Core10.Pass.createRenderPass', but includes extensible
-- sub-structures that include @sType@ and @pNext@ parameters, allowing
-- them to be more easily extended.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateRenderPass2-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateRenderPass2-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'RenderPassCreateInfo2'
--     structure
--
-- -   #VUID-vkCreateRenderPass2-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateRenderPass2-pRenderPass-parameter# @pRenderPass@
--     /must/ be a valid pointer to a 'Vulkan.Core10.Handles.RenderPass'
--     handle
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.RenderPass',
-- 'RenderPassCreateInfo2'
createRenderPass2 :: forall a io
                   . (Extendss RenderPassCreateInfo2 a, PokeChain a, MonadIO io)
                  => -- | @device@ is the logical device that creates the render pass.
                     Device
                  -> -- | @pCreateInfo@ is a pointer to a 'RenderPassCreateInfo2' structure
                     -- describing the parameters of the render pass.
                     (RenderPassCreateInfo2 a)
                  -> -- | @pAllocator@ controls host memory allocation as described in the
                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                     -- chapter.
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (RenderPass)
createRenderPass2 device createInfo allocator = liftIO . evalContT $ do
  let vkCreateRenderPass2Ptr = pVkCreateRenderPass2 (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateRenderPass2 is null" Nothing Nothing
  let vkCreateRenderPass2' = mkVkCreateRenderPass2 vkCreateRenderPass2Ptr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPRenderPass <- ContT $ bracket (callocBytes @RenderPass 8) free
  r <- lift $ traceAroundEvent "vkCreateRenderPass2" (vkCreateRenderPass2'
                                                        (deviceHandle (device))
                                                        (forgetExtensions pCreateInfo)
                                                        pAllocator
                                                        (pPRenderPass))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pRenderPass <- lift $ peek @RenderPass pPRenderPass
  pure $ (pRenderPass)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderPass2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct RenderPassBeginInfo) -> Ptr SubpassBeginInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct RenderPassBeginInfo) -> Ptr SubpassBeginInfo -> IO ()

-- | vkCmdBeginRenderPass2 - Begin a new render pass
--
-- = Description
--
-- After beginning a render pass instance, the command buffer is ready to
-- record the commands for the first subpass of that render pass.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginRenderPass2-framebuffer-02779# Both the
--     @framebuffer@ and @renderPass@ members of @pRenderPassBegin@ /must/
--     have been created on the same 'Vulkan.Core10.Handles.Device' that
--     @commandBuffer@ was allocated on
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-03094# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-03096# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-02844# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass2-stencilInitialLayout-02845# If any of
--     the @stencilInitialLayout@ or @stencilFinalLayout@ member of the
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structures or the @stencilLayout@ member of the
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-03097# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT' or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-03098# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-03099# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-03100# If the
--     @initialLayout@ member of any of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures specified when
--     creating the render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is not
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED', then each
--     such @initialLayout@ /must/ be equal to the current layout of the
--     corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@
--
-- -   #VUID-vkCmdBeginRenderPass2-srcStageMask-06453# The @srcStageMask@
--     members of any element of the @pDependencies@ member of
--     'Vulkan.Core10.Pass.RenderPassCreateInfo' used to create
--     @renderPass@ /must/ be supported by the capabilities of the queue
--     family identified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' used to create the
--     command pool which @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdBeginRenderPass2-dstStageMask-06454# The @dstStageMask@
--     members of any element of the @pDependencies@ member of
--     'Vulkan.Core10.Pass.RenderPassCreateInfo' used to create
--     @renderPass@ /must/ be supported by the capabilities of the queue
--     family identified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' used to create the
--     command pool which @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdBeginRenderPass2-framebuffer-02533# For any attachment in
--     @framebuffer@ that is used by @renderPass@ and is bound to memory
--     locations that are also bound to another attachment used by
--     @renderPass@, and if at least one of those uses causes either
--     attachment to be written to, both attachments /must/ have had the
--     'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
--     set
--
-- -   #VUID-vkCmdBeginRenderPass2-framebuffer-09046# If any attachments
--     specified in @framebuffer@ are used by @renderPass@ and are bound to
--     overlapping memory locations, there /must/ be only one that is used
--     as a color attachment, depth\/stencil, or resolve attachment in any
--     subpass
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-07002# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including either the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     and either the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     or 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     usage bits
--
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-07003# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     usage bit
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginRenderPass2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginRenderPass2-pRenderPassBegin-parameter#
--     @pRenderPassBegin@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo' structure
--
-- -   #VUID-vkCmdBeginRenderPass2-pSubpassBeginInfo-parameter#
--     @pSubpassBeginInfo@ /must/ be a valid pointer to a valid
--     'SubpassBeginInfo' structure
--
-- -   #VUID-vkCmdBeginRenderPass2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginRenderPass2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginRenderPass2-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdBeginRenderPass2-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- -   #VUID-vkCmdBeginRenderPass2-bufferlevel# @commandBuffer@ /must/ be a
--     primary 'Vulkan.Core10.Handles.CommandBuffer'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | Synchronization                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
-- 'SubpassBeginInfo'
cmdBeginRenderPass2 :: forall a io
                     . (Extendss RenderPassBeginInfo a, PokeChain a, MonadIO io)
                    => -- | @commandBuffer@ is the command buffer in which to record the command.
                       CommandBuffer
                    -> -- | @pRenderPassBegin@ is a pointer to a
                       -- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo' structure
                       -- specifying the render pass to begin an instance of, and the framebuffer
                       -- the instance uses.
                       (RenderPassBeginInfo a)
                    -> -- | @pSubpassBeginInfo@ is a pointer to a 'SubpassBeginInfo' structure
                       -- containing information about the subpass which is about to begin
                       -- rendering.
                       SubpassBeginInfo
                    -> io ()
cmdBeginRenderPass2 commandBuffer
                      renderPassBegin
                      subpassBeginInfo = liftIO . evalContT $ do
  let vkCmdBeginRenderPass2Ptr = pVkCmdBeginRenderPass2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBeginRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginRenderPass2 is null" Nothing Nothing
  let vkCmdBeginRenderPass2' = mkVkCmdBeginRenderPass2 vkCmdBeginRenderPass2Ptr
  pRenderPassBegin <- ContT $ withCStruct (renderPassBegin)
  pSubpassBeginInfo <- ContT $ withCStruct (subpassBeginInfo)
  lift $ traceAroundEvent "vkCmdBeginRenderPass2" (vkCmdBeginRenderPass2'
                                                     (commandBufferHandle (commandBuffer))
                                                     (forgetExtensions pRenderPassBegin)
                                                     pSubpassBeginInfo)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginRenderPass2' and 'cmdEndRenderPass2'
--
-- Note that 'cmdEndRenderPass2' is *not* called if an exception is thrown
-- by the inner action.
cmdUseRenderPass2 :: forall a io r . (Extendss RenderPassBeginInfo a, Extendss SubpassEndInfo a, PokeChain a, PokeChain a, MonadIO io) => CommandBuffer -> RenderPassBeginInfo a -> SubpassBeginInfo -> SubpassEndInfo a -> io r -> io r
cmdUseRenderPass2 commandBuffer
                    pRenderPassBegin
                    pSubpassBeginInfo
                    pSubpassEndInfo
                    a =
  (cmdBeginRenderPass2 commandBuffer
                         pRenderPassBegin
                         pSubpassBeginInfo) *> a <* (cmdEndRenderPass2 commandBuffer
                                                                         pSubpassEndInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr SubpassBeginInfo -> Ptr (SomeStruct SubpassEndInfo) -> IO ()) -> Ptr CommandBuffer_T -> Ptr SubpassBeginInfo -> Ptr (SomeStruct SubpassEndInfo) -> IO ()

-- | vkCmdNextSubpass2 - Transition to the next subpass of a render pass
--
-- = Description
--
-- 'cmdNextSubpass2' is semantically identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdNextSubpass', except that it is
-- extensible, and that @contents@ is provided as part of an extensible
-- structure instead of as a flat parameter.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdNextSubpass2-None-03102# The current subpass index /must/
--     be less than the number of subpasses in the render pass minus one
--
-- -   #VUID-vkCmdNextSubpass2-None-02350# This command /must/ not be
--     recorded when transform feedback is active
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdNextSubpass2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdNextSubpass2-pSubpassBeginInfo-parameter#
--     @pSubpassBeginInfo@ /must/ be a valid pointer to a valid
--     'SubpassBeginInfo' structure
--
-- -   #VUID-vkCmdNextSubpass2-pSubpassEndInfo-parameter# @pSubpassEndInfo@
--     /must/ be a valid pointer to a valid 'SubpassEndInfo' structure
--
-- -   #VUID-vkCmdNextSubpass2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdNextSubpass2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdNextSubpass2-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdNextSubpass2-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- -   #VUID-vkCmdNextSubpass2-bufferlevel# @commandBuffer@ /must/ be a
--     primary 'Vulkan.Core10.Handles.CommandBuffer'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | Synchronization                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'SubpassBeginInfo',
-- 'SubpassEndInfo'
cmdNextSubpass2 :: forall a io
                 . (Extendss SubpassEndInfo a, PokeChain a, MonadIO io)
                => -- | @commandBuffer@ is the command buffer in which to record the command.
                   CommandBuffer
                -> -- | @pSubpassBeginInfo@ is a pointer to a 'SubpassBeginInfo' structure
                   -- containing information about the subpass which is about to begin
                   -- rendering.
                   SubpassBeginInfo
                -> -- | @pSubpassEndInfo@ is a pointer to a 'SubpassEndInfo' structure
                   -- containing information about how the previous subpass will be ended.
                   (SubpassEndInfo a)
                -> io ()
cmdNextSubpass2 commandBuffer
                  subpassBeginInfo
                  subpassEndInfo = liftIO . evalContT $ do
  let vkCmdNextSubpass2Ptr = pVkCmdNextSubpass2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdNextSubpass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdNextSubpass2 is null" Nothing Nothing
  let vkCmdNextSubpass2' = mkVkCmdNextSubpass2 vkCmdNextSubpass2Ptr
  pSubpassBeginInfo <- ContT $ withCStruct (subpassBeginInfo)
  pSubpassEndInfo <- ContT $ withCStruct (subpassEndInfo)
  lift $ traceAroundEvent "vkCmdNextSubpass2" (vkCmdNextSubpass2'
                                                 (commandBufferHandle (commandBuffer))
                                                 pSubpassBeginInfo
                                                 (forgetExtensions pSubpassEndInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct SubpassEndInfo) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct SubpassEndInfo) -> IO ()

-- | vkCmdEndRenderPass2 - End the current render pass
--
-- = Description
--
-- 'cmdEndRenderPass2' is semantically identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndRenderPass', except that it
-- is extensible.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndRenderPass2-None-03103# The current subpass index
--     /must/ be equal to the number of subpasses in the render pass minus
--     one
--
-- -   #VUID-vkCmdEndRenderPass2-None-02352# This command /must/ not be
--     recorded when transform feedback is active
--
-- -   #VUID-vkCmdEndRenderPass2-None-06171# The current render pass
--     instance /must/ not have been begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdEndRenderPass2-None-07005# If
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery'* was called
--     within a subpass of the render pass, the corresponding
--     'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery'* /must/ have been
--     called subsequently within the same subpass
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndRenderPass2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndRenderPass2-pSubpassEndInfo-parameter#
--     @pSubpassEndInfo@ /must/ be a valid pointer to a valid
--     'SubpassEndInfo' structure
--
-- -   #VUID-vkCmdEndRenderPass2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndRenderPass2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdEndRenderPass2-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdEndRenderPass2-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- -   #VUID-vkCmdEndRenderPass2-bufferlevel# @commandBuffer@ /must/ be a
--     primary 'Vulkan.Core10.Handles.CommandBuffer'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | Synchronization                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'SubpassEndInfo'
cmdEndRenderPass2 :: forall a io
                   . (Extendss SubpassEndInfo a, PokeChain a, MonadIO io)
                  => -- | @commandBuffer@ is the command buffer in which to end the current render
                     -- pass instance.
                     CommandBuffer
                  -> -- | @pSubpassEndInfo@ is a pointer to a 'SubpassEndInfo' structure
                     -- containing information about how the last subpass will be ended.
                     (SubpassEndInfo a)
                  -> io ()
cmdEndRenderPass2 commandBuffer subpassEndInfo = liftIO . evalContT $ do
  let vkCmdEndRenderPass2Ptr = pVkCmdEndRenderPass2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdEndRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndRenderPass2 is null" Nothing Nothing
  let vkCmdEndRenderPass2' = mkVkCmdEndRenderPass2 vkCmdEndRenderPass2Ptr
  pSubpassEndInfo <- ContT $ withCStruct (subpassEndInfo)
  lift $ traceAroundEvent "vkCmdEndRenderPass2" (vkCmdEndRenderPass2'
                                                   (commandBufferHandle (commandBuffer))
                                                   (forgetExtensions pSubpassEndInfo))
  pure $ ()


-- | VkAttachmentDescription2 - Structure specifying an attachment
-- description
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Vulkan.Core10.Pass.AttachmentDescription' have the identical effect to
-- those parameters.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
-- feature is enabled, and @format@ is a depth\/stencil format,
-- @initialLayout@ and @finalLayout@ /can/ be set to a layout that only
-- specifies the layout of the depth aspect.
--
-- If the @pNext@ chain includes a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
-- structure, then the @stencilInitialLayout@ and @stencilFinalLayout@
-- members specify the initial and final layouts of the stencil aspect of a
-- depth\/stencil format, and @initialLayout@ and @finalLayout@ only apply
-- to the depth aspect. For depth-only formats, the
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
-- structure is ignored. For stencil-only formats, the initial and final
-- layouts of the stencil aspect are taken from the
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
-- structure if present, or @initialLayout@ and @finalLayout@ if not
-- present.
--
-- If @format@ is a depth\/stencil format, and either @initialLayout@ or
-- @finalLayout@ does not specify a layout for the stencil aspect, then the
-- application /must/ specify the initial and final layouts of the stencil
-- aspect by including a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
-- structure in the @pNext@ chain.
--
-- @loadOp@ and @storeOp@ are ignored for fragment shading rate
-- attachments. No access to the shading rate attachment is performed in
-- @loadOp@ and @storeOp@. Instead, access to
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR'
-- is performed as fragments are rasterized.
--
-- == Valid Usage
--
-- -   #VUID-VkAttachmentDescription2-format-06699# If @format@ includes a
--     color or depth component and @loadOp@ is
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_LOAD', then
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED'
--
-- -   #VUID-VkAttachmentDescription2-finalLayout-00843# @finalLayout@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkAttachmentDescription2-format-03280# If @format@ is a color
--     format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03281# If @format@ is a
--     depth\/stencil format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03282# If @format@ is a color
--     format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03283# If @format@ is a
--     depth\/stencil format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-06487# If @format@ is a color
--     format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-06488# If @format@ is a color
--     format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-separateDepthStencilLayouts-03284# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--
-- -   #VUID-VkAttachmentDescription2-separateDepthStencilLayouts-03285# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--
-- -   #VUID-VkAttachmentDescription2-format-03286# If @format@ is a color
--     format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03287# If @format@ is a color
--     format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-06906# If @format@ is a
--     depth\/stencil format which includes both depth and stencil
--     components, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-06907# If @format@ is a
--     depth\/stencil format which includes both depth and stencil
--     components, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03290# If @format@ is a
--     depth\/stencil format which includes only the depth component,
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03291# If @format@ is a
--     depth\/stencil format which includes only the depth component,
--     @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-synchronization2-06908# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @initialLayout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkAttachmentDescription2-synchronization2-06909# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @finalLayout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkAttachmentDescription2-attachmentFeedbackLoopLayout-07309#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFeedbackLoopLayout attachmentFeedbackLoopLayout>
--     feature is not enabled, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--
-- -   #VUID-VkAttachmentDescription2-attachmentFeedbackLoopLayout-07310#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFeedbackLoopLayout attachmentFeedbackLoopLayout>
--     feature is not enabled, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--
-- -   #VUID-VkAttachmentDescription2-samples-08745# @samples@ /must/ be a
--     bit value that is set in @imageCreateSampleCounts@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     for the given @format@
--
-- -   #VUID-VkAttachmentDescription2-pNext-06704# If the @pNext@ chain
--     does not include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structure, @format@ includes a stencil component, and
--     @stencilLoadOp@ is
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_LOAD', then
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED'
--
-- -   #VUID-VkAttachmentDescription2-pNext-06705# If the @pNext@ chain
--     includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structure, @format@ includes a stencil component, and
--     @stencilLoadOp@ is
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_LOAD', then
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'::@stencilInitialLayout@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED'
--
-- -   #VUID-VkAttachmentDescription2-format-06249# If @format@ is a
--     depth\/stencil format which includes both depth and stencil
--     components, and @initialLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structure
--
-- -   #VUID-VkAttachmentDescription2-format-06250# If @format@ is a
--     depth\/stencil format which includes both depth and stencil
--     components, and @finalLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structure
--
-- -   #VUID-VkAttachmentDescription2-format-06247# If the @pNext@ chain
--     does not include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structure and @format@ only includes a stencil component,
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-06248# If the @pNext@ chain
--     does not include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structure and @format@ only includes a stencil component,
--     @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-09332# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is not enabled, @format@ /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-VkAttachmentDescription2-format-09334# If @format@ is
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', there /must/ be a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure in the @pNext@ chain with a @externalFormat@ that is not
--     equal to @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAttachmentDescription2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2'
--
-- -   #VUID-VkAttachmentDescription2-pNext-pNext# Each @pNext@ member of
--     any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     or
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--
-- -   #VUID-VkAttachmentDescription2-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkAttachmentDescription2-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlagBits'
--     values
--
-- -   #VUID-VkAttachmentDescription2-format-parameter# @format@ /must/ be
--     a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkAttachmentDescription2-samples-parameter# @samples@ /must/
--     be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkAttachmentDescription2-loadOp-parameter# @loadOp@ /must/ be
--     a valid 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp'
--     value
--
-- -   #VUID-VkAttachmentDescription2-storeOp-parameter# @storeOp@ /must/
--     be a valid 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
--     value
--
-- -   #VUID-VkAttachmentDescription2-stencilLoadOp-parameter#
--     @stencilLoadOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp' value
--
-- -   #VUID-VkAttachmentDescription2-stencilStoreOp-parameter#
--     @stencilStoreOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp' value
--
-- -   #VUID-VkAttachmentDescription2-initialLayout-parameter#
--     @initialLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkAttachmentDescription2-finalLayout-parameter# @finalLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlags',
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp',
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'RenderPassCreateInfo2',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AttachmentDescription2 (es :: [Type]) = AttachmentDescription2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlagBits'
    -- specifying additional properties of the attachment.
    flags :: AttachmentDescriptionFlags
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' value specifying the
    -- format of the image that will be used for the attachment.
    format :: Format
  , -- | @samples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    -- specifying the number of samples of the image.
    samples :: SampleCountFlagBits
  , -- | @loadOp@ is a 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp'
    -- value specifying how the contents of color and depth components of the
    -- attachment are treated at the beginning of the subpass where it is first
    -- used.
    loadOp :: AttachmentLoadOp
  , -- | @storeOp@ is a 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
    -- value specifying how the contents of color and depth components of the
    -- attachment are treated at the end of the subpass where it is last used.
    storeOp :: AttachmentStoreOp
  , -- | @stencilLoadOp@ is a
    -- 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp' value specifying
    -- how the contents of stencil components of the attachment are treated at
    -- the beginning of the subpass where it is first used.
    stencilLoadOp :: AttachmentLoadOp
  , -- | @stencilStoreOp@ is a
    -- 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp' value
    -- specifying how the contents of stencil components of the attachment are
    -- treated at the end of the last subpass where it is used.
    stencilStoreOp :: AttachmentStoreOp
  , -- | @initialLayout@ is the layout the attachment image subresource will be
    -- in when a render pass instance begins.
    initialLayout :: ImageLayout
  , -- | @finalLayout@ is the layout the attachment image subresource will be
    -- transitioned to when a render pass instance ends.
    finalLayout :: ImageLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentDescription2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (AttachmentDescription2 es)

instance Extensible AttachmentDescription2 where
  extensibleTypeName = "AttachmentDescription2"
  setNext AttachmentDescription2{..} next' = AttachmentDescription2{next = next', ..}
  getNext AttachmentDescription2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AttachmentDescription2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @AttachmentDescriptionStencilLayout = Just f
    | Just Refl <- eqT @e @ExternalFormatANDROID = Just f
    | otherwise = Nothing

instance ( Extendss AttachmentDescription2 es
         , PokeChain es ) => ToCStruct (AttachmentDescription2 es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentDescription2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr AttachmentDescriptionFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 24 :: Ptr SampleCountFlagBits)) (samples)
    lift $ poke ((p `plusPtr` 28 :: Ptr AttachmentLoadOp)) (loadOp)
    lift $ poke ((p `plusPtr` 32 :: Ptr AttachmentStoreOp)) (storeOp)
    lift $ poke ((p `plusPtr` 36 :: Ptr AttachmentLoadOp)) (stencilLoadOp)
    lift $ poke ((p `plusPtr` 40 :: Ptr AttachmentStoreOp)) (stencilStoreOp)
    lift $ poke ((p `plusPtr` 44 :: Ptr ImageLayout)) (initialLayout)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (finalLayout)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr SampleCountFlagBits)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr AttachmentLoadOp)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr AttachmentStoreOp)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr AttachmentLoadOp)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr AttachmentStoreOp)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (zero)
    lift $ f

instance ( Extendss AttachmentDescription2 es
         , PeekChain es ) => FromCStruct (AttachmentDescription2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @AttachmentDescriptionFlags ((p `plusPtr` 16 :: Ptr AttachmentDescriptionFlags))
    format <- peek @Format ((p `plusPtr` 20 :: Ptr Format))
    samples <- peek @SampleCountFlagBits ((p `plusPtr` 24 :: Ptr SampleCountFlagBits))
    loadOp <- peek @AttachmentLoadOp ((p `plusPtr` 28 :: Ptr AttachmentLoadOp))
    storeOp <- peek @AttachmentStoreOp ((p `plusPtr` 32 :: Ptr AttachmentStoreOp))
    stencilLoadOp <- peek @AttachmentLoadOp ((p `plusPtr` 36 :: Ptr AttachmentLoadOp))
    stencilStoreOp <- peek @AttachmentStoreOp ((p `plusPtr` 40 :: Ptr AttachmentStoreOp))
    initialLayout <- peek @ImageLayout ((p `plusPtr` 44 :: Ptr ImageLayout))
    finalLayout <- peek @ImageLayout ((p `plusPtr` 48 :: Ptr ImageLayout))
    pure $ AttachmentDescription2
             next
             flags
             format
             samples
             loadOp
             storeOp
             stencilLoadOp
             stencilStoreOp
             initialLayout
             finalLayout

instance es ~ '[] => Zero (AttachmentDescription2 es) where
  zero = AttachmentDescription2
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


-- | VkAttachmentReference2 - Structure specifying an attachment reference
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Vulkan.Core10.Pass.AttachmentReference' have the identical effect to
-- those parameters.
--
-- @aspectMask@ is ignored when this structure is used to describe anything
-- other than an input attachment reference.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
-- feature is enabled, and @attachment@ has a depth\/stencil format,
-- @layout@ /can/ be set to a layout that only specifies the layout of the
-- depth aspect.
--
-- If @layout@ only specifies the layout of the depth aspect of the
-- attachment, the layout of the stencil aspect is specified by the
-- @stencilLayout@ member of a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout'
-- structure included in the @pNext@ chain. Otherwise, @layout@ describes
-- the layout for all relevant image aspects.
--
-- == Valid Usage
--
-- -   #VUID-VkAttachmentReference2-layout-03077# If @attachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', @layout@ /must/ not
--     be 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR'
--
-- -   #VUID-VkAttachmentReference2-separateDepthStencilLayouts-03313# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, and @attachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', @layout@ /must/ not
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--
-- -   #VUID-VkAttachmentReference2-synchronization2-06910# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @layout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkAttachmentReference2-attachmentFeedbackLoopLayout-07311# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFeedbackLoopLayout attachmentFeedbackLoopLayout>
--     feature is not enabled, @layout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAttachmentReference2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2'
--
-- -   #VUID-VkAttachmentReference2-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout'
--
-- -   #VUID-VkAttachmentReference2-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkAttachmentReference2-layout-parameter# @layout@ /must/ be a
--     valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'SubpassDescription2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
data AttachmentReference2 (es :: [Type]) = AttachmentReference2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @attachment@ is either an integer value identifying an attachment at the
    -- corresponding index in 'RenderPassCreateInfo2'::@pAttachments@, or
    -- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' to signify that this
    -- attachment is not used.
    attachment :: Word32
  , -- | @layout@ is a 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
    -- specifying the layout the attachment uses during the subpass.
    layout :: ImageLayout
  , -- | @aspectMask@ is a mask of which aspect(s) /can/ be accessed within the
    -- specified subpass as an input attachment.
    aspectMask :: ImageAspectFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentReference2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (AttachmentReference2 es)

instance Extensible AttachmentReference2 where
  extensibleTypeName = "AttachmentReference2"
  setNext AttachmentReference2{..} next' = AttachmentReference2{next = next', ..}
  getNext AttachmentReference2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AttachmentReference2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @AttachmentReferenceStencilLayout = Just f
    | otherwise = Nothing

instance ( Extendss AttachmentReference2 es
         , PokeChain es ) => ToCStruct (AttachmentReference2 es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentReference2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (attachment)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageLayout)) (layout)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageAspectFlags)) (aspectMask)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageAspectFlags)) (zero)
    lift $ f

instance ( Extendss AttachmentReference2 es
         , PeekChain es ) => FromCStruct (AttachmentReference2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    attachment <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    layout <- peek @ImageLayout ((p `plusPtr` 20 :: Ptr ImageLayout))
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 24 :: Ptr ImageAspectFlags))
    pure $ AttachmentReference2
             next attachment layout aspectMask

instance es ~ '[] => Zero (AttachmentReference2 es) where
  zero = AttachmentReference2
           ()
           zero
           zero
           zero


-- | VkSubpassDescription2 - Structure specifying a subpass description
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Vulkan.Core10.Pass.SubpassDescription' have the identical effect to
-- those parameters.
--
-- @viewMask@ has the same effect for the described subpass as
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pViewMasks@
-- has on each corresponding subpass.
--
-- If a
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'
-- structure is included in the @pNext@ chain,
-- @pFragmentShadingRateAttachment@ is not @NULL@, and its @attachment@
-- member is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the
-- identified attachment defines a fragment shading rate attachment for
-- that subpass.
--
-- If any element of @pResolveAttachments@ is an image specified with an
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID',
-- values in the corresponding color attachment will be resolved to the
-- resolve attachment in the same manner as specified for
-- <VkResolveModeFlagBits.html >.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-nullColorAttachmentWithExternalFormatResolve nullColorAttachmentWithExternalFormatResolve>
-- limit is 'Vulkan.Core10.FundamentalTypes.TRUE', values in the color
-- attachment will be loaded from the resolve attachment at the start of
-- rendering, and /may/ also be reloaded any time after a resolve occurs or
-- the resolve attachment is written to; if this occurs it /must/
-- happen-before any writes to the color attachment are performed which
-- happen-after the resolve that triggers this. If any color component in
-- the external format is subsampled, values will be read from the nearest
-- sample in the image when they are loaded. If the color attachment is
-- also used as an input attachment, the same behavior applies.
--
-- Setting the color attachment to
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' when an external resolve
-- attachment is used and the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-nullColorAttachmentWithExternalFormatResolve nullColorAttachmentWithExternalFormatResolve>
-- limit is 'Vulkan.Core10.FundamentalTypes.TRUE' will not result in color
-- attachment writes to be discarded for that attachment.
--
-- When
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-nullColorAttachmentWithExternalFormatResolve nullColorAttachmentWithExternalFormatResolve>
-- is 'Vulkan.Core10.FundamentalTypes.TRUE', the color output from the
-- subpass can still be read via an input attachment; but the application
-- cannot bind an image view for the color attachment as there is no such
-- image view bound. Instead to access the data as an input attachment
-- applications /can/ use the resolve attachment in its place - using the
-- resolve attachment image for the descriptor, and setting the
-- corresponding element of @pInputAttachments@ to the index of the resolve
-- attachment.
--
-- Loads or input attachment reads from the resolve attachment are
-- performed as if using a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
-- with the following parameters:
--
-- > VkSamplerYcbcrConversionCreateInfo createInfo = {
-- >     .sType = VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO,
-- >     .pNext = NULL,
-- >     .format = VK_FORMAT_UNDEFINED,
-- >     .ycbcrModel = VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY,
-- >     .ycbcrRange = VK_SAMPLER_YCBCR_RANGE_ITU_FULL,
-- >     .components = {
-- >         .r = VK_COMPONENT_SWIZZLE_B
-- >         .g = VK_COMPONENT_SWIZZLE_R
-- >         .b = VK_COMPONENT_SWIZZLE_G
-- >         .a = VK_COMPONENT_SWIZZLE_IDENTITY},
-- >     .xChromaOffset = properties.chromaOffsetX,
-- >     .yChromaOffset = properties.chromaOffsetY,
-- >     .chromaFilter = ename:VK_FILTER_NEAREST,
-- >     .forceExplicitReconstruction = ... };
--
-- where @properties@ is equal to
-- 'Vulkan.Extensions.VK_ANDROID_external_format_resolve.PhysicalDeviceExternalFormatResolvePropertiesANDROID'
-- returned by the device and @forceExplicitReconstruction@ is effectively
-- ignored as the
-- 'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY'
-- model is used. The applied swizzle is the same effective swizzle that
-- would be applied by the
-- 'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY'
-- model, but no range expansion is applied.
--
-- == Valid Usage
--
-- -   #VUID-VkSubpassDescription2-attachment-06912# If the @attachment@
--     member of an element of @pInputAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06913# If the @attachment@
--     member of an element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06914# If the @attachment@
--     member of an element of @pResolveAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06915# If the @attachment@
--     member of @pDepthStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', ts @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06916# If the @attachment@
--     member of an element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06917# If the @attachment@
--     member of an element of @pResolveAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06918# If the @attachment@
--     member of an element of @pInputAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06919# If the @attachment@
--     member of an element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06920# If the @attachment@
--     member of an element of @pResolveAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-attachment-06921# If the @attachment@
--     member of an element of @pInputAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--
-- -   #VUID-VkSubpassDescription2-attachment-06922# If the @attachment@
--     member of an element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkSubpassDescription2-attachment-06923# If the @attachment@
--     member of an element of @pResolveAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkSubpassDescription2-attachment-06251# If the @attachment@
--     member of @pDepthStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and its @pNext@ chain
--     includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout'
--     structure, the @layout@ member of @pDepthStencilAttachment@ /must/
--     not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkSubpassDescription2-pipelineBindPoint-04953#
--     @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--     or
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI'
--
-- -   #VUID-VkSubpassDescription2-colorAttachmentCount-03063#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxColorAttachments@
--
-- -   #VUID-VkSubpassDescription2-loadOp-03064# If the first use of an
--     attachment in this render pass is as an input attachment, and the
--     attachment is not also used as a color or depth\/stencil attachment
--     in the same subpass, then @loadOp@ /must/ not be
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   #VUID-VkSubpassDescription2-pResolveAttachments-03067# If
--     @pResolveAttachments@ is not @NULL@, each resolve attachment that is
--     not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have a
--     sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkSubpassDescription2-externalFormatResolve-09335# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is not enabled and @pResolveAttachments@ is not @NULL@, for each
--     resolve attachment that does not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the corresponding
--     color attachment /must/ not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-nullColorAttachmentWithExternalFormatResolve-09336#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-nullColorAttachmentWithExternalFormatResolve nullColorAttachmentWithExternalFormatResolve>
--     property is 'Vulkan.Core10.FundamentalTypes.FALSE' and
--     @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that has a format of 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     the corresponding color attachment /must/ not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-nullColorAttachmentWithExternalFormatResolve-09337#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-nullColorAttachmentWithExternalFormatResolve nullColorAttachmentWithExternalFormatResolve>
--     property is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that has a format of 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     the corresponding color attachment /must/ have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-externalFormatResolve-09338# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is not enabled and @pResolveAttachments@ is not @NULL@, for each
--     resolve attachment that is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the corresponding
--     color attachment /must/ not have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkSubpassDescription2-externalFormatResolve-09339# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is not enabled, each element of @pResolveAttachments@ /must/ have
--     the same 'Vulkan.Core10.Enums.Format.Format' as its corresponding
--     color attachment
--
-- -   #VUID-VkSubpassDescription2-multisampledRenderToSingleSampled-06869#
--     If none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature are enabled, all attachments in @pColorAttachments@ that are
--     not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have the
--     same sample count
--
-- -   #VUID-VkSubpassDescription2-pInputAttachments-02897# All attachments
--     in @pInputAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and any of the
--     following is true:
--
--     -   the
--         <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--         feature is not enabled
--
--     -   the
--         <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-nullColorAttachmentWithExternalFormatResolve nullColorAttachmentWithExternalFormatResolve>
--         property is 'Vulkan.Core10.FundamentalTypes.FALSE'
--
--     -   does not have a non-zero value of
--         'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--
--     /must/ have image formats whose
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain at least
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription2-pColorAttachments-02898# All attachments
--     in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription2-pResolveAttachments-09343# All
--     attachments in @pResolveAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and do not have an
--     image format of 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' /must/
--     have image formats whose
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription2-pDepthStencilAttachment-02900# If
--     @pDepthStencilAttachment@ is not @NULL@ and the attachment is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' then it /must/ have
--     an image format whose
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription2-linearColorAttachment-06499# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-linearColorAttachment linearColorAttachment>
--     feature is enabled and the image is created with
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', all
--     attachments in @pInputAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
--
-- -   #VUID-VkSubpassDescription2-linearColorAttachment-06500# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-linearColorAttachment linearColorAttachment>
--     feature is enabled and the image is created with
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', all
--     attachments in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
--
-- -   #VUID-VkSubpassDescription2-linearColorAttachment-06501# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-linearColorAttachment linearColorAttachment>
--     feature is enabled and the image is created with
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', all
--     attachments in @pResolveAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
--
-- -   #VUID-VkSubpassDescription2-pColorAttachments-03070# If the
--     @VK_AMD_mixed_attachment_samples@ extension is enabled, all
--     attachments in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have a sample
--     count that is smaller than or equal to the sample count of
--     @pDepthStencilAttachment@ if it is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-pNext-06870# If the @pNext@ chain
--     includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with @multisampledRenderToSingleSampledEnable@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then all attachments in
--     @pColorAttachments@ and @pDepthStencilAttachment@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have a sample
--     count that is either
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' or
--     equal to
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'::@rasterizationSamples@
--
-- -   #VUID-VkSubpassDescription2-pNext-06871# If the @pNext@ chain
--     includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with @multisampledRenderToSingleSampledEnable@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE', and @pDepthStencilAttachment@
--     is not @NULL@, does not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', and has a sample
--     count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT', the
--     @pNext@ chain /must/ also include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
--     structure with @pDepthStencilResolveAttachment@ that is either
--     @NULL@ or has the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-multisampledRenderToSingleSampled-06872#
--     If none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature are enabled, all attachments in @pDepthStencilAttachment@ or
--     @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have the same
--     sample count
--
-- -   #VUID-VkSubpassDescription2-attachment-03073# Each element of
--     @pPreserveAttachments@ /must/ not be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-pPreserveAttachments-03074# Each element
--     of @pPreserveAttachments@ /must/ not also be an element of any other
--     member of the subpass description
--
-- -   #VUID-VkSubpassDescription2-layout-02528# If any attachment is used
--     by more than one 'AttachmentReference2' member, then each use /must/
--     use the same @layout@
--
-- -   #VUID-VkSubpassDescription2-flags-03076# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX',
--     it /must/ also include
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
--
-- -   #VUID-VkSubpassDescription2-attachment-02799# If the @attachment@
--     member of any element of @pInputAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then the
--     @aspectMask@ member /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits'
--
-- -   #VUID-VkSubpassDescription2-attachment-02800# If the @attachment@
--     member of any element of @pInputAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then the
--     @aspectMask@ member /must/ not be @0@
--
-- -   #VUID-VkSubpassDescription2-attachment-02801# If the @attachment@
--     member of any element of @pInputAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then the
--     @aspectMask@ member /must/ not include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_METADATA_BIT'
--
-- -   #VUID-VkSubpassDescription2-attachment-04563# If the @attachment@
--     member of any element of @pInputAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then the
--     @aspectMask@ member /must/ not include
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index /i/
--
-- -   #VUID-VkSubpassDescription2-pDepthStencilAttachment-04440# An
--     attachment /must/ not be used in both @pDepthStencilAttachment@ and
--     @pColorAttachments@
--
-- -   #VUID-VkSubpassDescription2-multiview-06558# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview multiview>
--     feature is not enabled, @viewMask@ /must/ be @0@
--
-- -   #VUID-VkSubpassDescription2-viewMask-06706# The index of the most
--     significant bit in @viewMask@ /must/ be less than
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxMultiviewViewCount maxMultiviewViewCount>
--
-- -   #VUID-VkSubpassDescription2-externalFormatResolve-09344# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is enabled, @pResolveAttachments@ is not @NULL@, and
--     @colorAttachmentCount@ is not @1@, any element of
--     @pResolveAttachments@ that is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', /must/ not have a
--     format of 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-VkSubpassDescription2-externalFormatResolve-09345# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is enabled, @pResolveAttachments@ is not @NULL@, any element of
--     @pResolveAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and has a format of
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', and the corresponding
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the color attachment
--     /must/ have a @samples@ value of @1@
--
-- -   #VUID-VkSubpassDescription2-externalFormatResolve-09346# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is enabled, @pResolveAttachments@ is not @NULL@, and any element of
--     @pResolveAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and has a format of
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', @viewMask@ /must/ be
--     @0@
--
-- -   #VUID-VkSubpassDescription2-externalFormatResolve-09347# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is enabled, @pResolveAttachments@ is not @NULL@, and any element of
--     @pResolveAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and has a format of
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'::@pFragmentShadingRateAttachment@
--     /must/ either be @NULL@ or a 'AttachmentReference2' structure with a
--     @attachment@ value of 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-externalFormatResolve-09348# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     is enabled, @pResolveAttachments@ is not @NULL@, and any element of
--     @pResolveAttachments@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and has a format of
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', elements of
--     @pInputAttachments@ referencing either a color attachment or resolve
--     attachment used in this subpass /must/ not include
--     @VK_IMAGE_ASPECT_PLANE_i_BIT@ for any index /i/ in its @aspectMask@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassDescription2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2'
--
-- -   #VUID-VkSubpassDescription2-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR',
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT',
--     'Vulkan.Extensions.VK_EXT_subpass_merge_feedback.RenderPassCreationControlEXT',
--     'Vulkan.Extensions.VK_EXT_subpass_merge_feedback.RenderPassSubpassFeedbackCreateInfoEXT',
--     or
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
--
-- -   #VUID-VkSubpassDescription2-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkSubpassDescription2-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits'
--     values
--
-- -   #VUID-VkSubpassDescription2-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-VkSubpassDescription2-pInputAttachments-parameter# If
--     @inputAttachmentCount@ is not @0@, @pInputAttachments@ /must/ be a
--     valid pointer to an array of @inputAttachmentCount@ valid
--     'AttachmentReference2' structures
--
-- -   #VUID-VkSubpassDescription2-pColorAttachments-parameter# If
--     @colorAttachmentCount@ is not @0@, @pColorAttachments@ /must/ be a
--     valid pointer to an array of @colorAttachmentCount@ valid
--     'AttachmentReference2' structures
--
-- -   #VUID-VkSubpassDescription2-pResolveAttachments-parameter# If
--     @colorAttachmentCount@ is not @0@, and @pResolveAttachments@ is not
--     @NULL@, @pResolveAttachments@ /must/ be a valid pointer to an array
--     of @colorAttachmentCount@ valid 'AttachmentReference2' structures
--
-- -   #VUID-VkSubpassDescription2-pDepthStencilAttachment-parameter# If
--     @pDepthStencilAttachment@ is not @NULL@, @pDepthStencilAttachment@
--     /must/ be a valid pointer to a valid 'AttachmentReference2'
--     structure
--
-- -   #VUID-VkSubpassDescription2-pPreserveAttachments-parameter# If
--     @preserveAttachmentCount@ is not @0@, @pPreserveAttachments@ /must/
--     be a valid pointer to an array of @preserveAttachmentCount@
--     @uint32_t@ values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'AttachmentReference2',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'RenderPassCreateInfo2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlags'
data SubpassDescription2 (es :: [Type]) = SubpassDescription2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits'
    -- specifying usage of the subpass.
    flags :: SubpassDescriptionFlags
  , -- | @pipelineBindPoint@ is a
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
    -- specifying the pipeline type supported for this subpass.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @viewMask@ is a bitfield of view indices describing which views
    -- rendering is broadcast to in this subpass, when multiview is enabled.
    viewMask :: Word32
  , -- | @pInputAttachments@ is a pointer to an array of 'AttachmentReference2'
    -- structures defining the input attachments for this subpass and their
    -- layouts.
    inputAttachments :: Vector (SomeStruct AttachmentReference2)
  , -- | @pColorAttachments@ is a pointer to an array of @colorAttachmentCount@
    -- 'AttachmentReference2' structures defining the color attachments for
    -- this subpass and their layouts.
    colorAttachments :: Vector (SomeStruct AttachmentReference2)
  , -- | @pResolveAttachments@ is @NULL@ or a pointer to an array of
    -- @colorAttachmentCount@ 'AttachmentReference2' structures defining the
    -- resolve attachments for this subpass and their layouts.
    resolveAttachments :: Vector (SomeStruct AttachmentReference2)
  , -- | @pDepthStencilAttachment@ is a pointer to a 'AttachmentReference2'
    -- structure specifying the depth\/stencil attachment for this subpass and
    -- its layout.
    depthStencilAttachment :: Maybe (SomeStruct AttachmentReference2)
  , -- | @pPreserveAttachments@ is a pointer to an array of
    -- @preserveAttachmentCount@ render pass attachment indices identifying
    -- attachments that are not used by this subpass, but whose contents /must/
    -- be preserved throughout the subpass.
    preserveAttachments :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDescription2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubpassDescription2 es)

instance Extensible SubpassDescription2 where
  extensibleTypeName = "SubpassDescription2"
  setNext SubpassDescription2{..} next' = SubpassDescription2{next = next', ..}
  getNext SubpassDescription2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubpassDescription2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassSubpassFeedbackCreateInfoEXT = Just f
    | Just Refl <- eqT @e @RenderPassCreationControlEXT = Just f
    | Just Refl <- eqT @e @MultisampledRenderToSingleSampledInfoEXT = Just f
    | Just Refl <- eqT @e @FragmentShadingRateAttachmentInfoKHR = Just f
    | Just Refl <- eqT @e @SubpassDescriptionDepthStencilResolve = Just f
    | otherwise = Nothing

instance ( Extendss SubpassDescription2 es
         , PokeChain es ) => ToCStruct (SubpassDescription2 es) where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDescription2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SubpassDescriptionFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (inputAttachments)) :: Word32))
    pPInputAttachments' <- ContT $ allocaBytes @(AttachmentReference2 _) ((Data.Vector.length (inputAttachments)) * 32)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPInputAttachments' `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) (inputAttachments)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (AttachmentReference2 _)))) (pPInputAttachments')
    let pColorAttachmentsLength = Data.Vector.length $ (colorAttachments)
    let pResolveAttachmentsLength = Data.Vector.length $ (resolveAttachments)
    lift $ unless (fromIntegral pResolveAttachmentsLength == pColorAttachmentsLength || pResolveAttachmentsLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pResolveAttachments and pColorAttachments must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral pColorAttachmentsLength :: Word32))
    pPColorAttachments' <- ContT $ allocaBytes @(AttachmentReference2 _) ((Data.Vector.length (colorAttachments)) * 32)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPColorAttachments' `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) (colorAttachments)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (AttachmentReference2 _)))) (pPColorAttachments')
    pResolveAttachments'' <- if Data.Vector.null (resolveAttachments)
      then pure nullPtr
      else do
        pPResolveAttachments <- ContT $ allocaBytes @(AttachmentReference2 _) (((Data.Vector.length (resolveAttachments))) * 32)
        Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPResolveAttachments `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) ((resolveAttachments))
        pure $ pPResolveAttachments
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (AttachmentReference2 _)))) pResolveAttachments''
    pDepthStencilAttachment'' <- case (depthStencilAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (AttachmentReference2 _)))) pDepthStencilAttachment''
    lift $ poke ((p `plusPtr` 72 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (preserveAttachments)) :: Word32))
    pPPreserveAttachments' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (preserveAttachments)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPreserveAttachments' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (preserveAttachments)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPPreserveAttachments')
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss SubpassDescription2 es
         , PeekChain es ) => FromCStruct (SubpassDescription2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SubpassDescriptionFlags ((p `plusPtr` 16 :: Ptr SubpassDescriptionFlags))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 20 :: Ptr PipelineBindPoint))
    viewMask <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    inputAttachmentCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pInputAttachments <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 32 :: Ptr (Ptr (AttachmentReference2 _))))
    pInputAttachments' <- generateM (fromIntegral inputAttachmentCount) (\i -> peekSomeCStruct (forgetExtensions ((pInputAttachments `advancePtrBytes` (32 * (i)) :: Ptr (AttachmentReference2 _)))))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pColorAttachments <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 48 :: Ptr (Ptr (AttachmentReference2 _))))
    pColorAttachments' <- generateM (fromIntegral colorAttachmentCount) (\i -> peekSomeCStruct (forgetExtensions ((pColorAttachments `advancePtrBytes` (32 * (i)) :: Ptr (AttachmentReference2 _)))))
    pResolveAttachments <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 56 :: Ptr (Ptr (AttachmentReference2 _))))
    let pResolveAttachmentsLength = if pResolveAttachments == nullPtr then 0 else (fromIntegral colorAttachmentCount)
    pResolveAttachments' <- generateM pResolveAttachmentsLength (\i -> peekSomeCStruct (forgetExtensions ((pResolveAttachments `advancePtrBytes` (32 * (i)) :: Ptr (AttachmentReference2 _)))))
    pDepthStencilAttachment <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 64 :: Ptr (Ptr (AttachmentReference2 _))))
    pDepthStencilAttachment' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pDepthStencilAttachment
    preserveAttachmentCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    pPreserveAttachments <- peek @(Ptr Word32) ((p `plusPtr` 80 :: Ptr (Ptr Word32)))
    pPreserveAttachments' <- generateM (fromIntegral preserveAttachmentCount) (\i -> peek @Word32 ((pPreserveAttachments `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ SubpassDescription2
             next
             flags
             pipelineBindPoint
             viewMask
             pInputAttachments'
             pColorAttachments'
             pResolveAttachments'
             pDepthStencilAttachment'
             pPreserveAttachments'

instance es ~ '[] => Zero (SubpassDescription2 es) where
  zero = SubpassDescription2
           ()
           zero
           zero
           zero
           mempty
           mempty
           mempty
           Nothing
           mempty


-- | VkSubpassDependency2 - Structure specifying a subpass dependency
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Vulkan.Core10.Pass.SubpassDependency' have the identical effect to
-- those parameters.
--
-- @viewOffset@ has the same effect for the described subpass dependency as
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pViewOffsets@
-- has on each corresponding subpass dependency.
--
-- If a
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.MemoryBarrier2' is
-- included in the @pNext@ chain, @srcStageMask@, @dstStageMask@,
-- @srcAccessMask@, and @dstAccessMask@ parameters are ignored. The
-- synchronization and access scopes instead are defined by the parameters
-- of 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.MemoryBarrier2'.
--
-- == Valid Usage
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-07318# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-03937# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @srcStageMask@ /must/ not be @0@
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-07949# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-07318# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-03937# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @dstStageMask@ /must/ not be @0@
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-07949# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkSubpassDependency2-srcSubpass-03084# @srcSubpass@ /must/ be
--     less than or equal to @dstSubpass@, unless one of them is
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', to avoid cyclic
--     dependencies and ensure a valid execution order
--
-- -   #VUID-VkSubpassDependency2-srcSubpass-03085# @srcSubpass@ and
--     @dstSubpass@ /must/ not both be equal to
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'
--
-- -   #VUID-VkSubpassDependency2-srcSubpass-06810# If @srcSubpass@ is
--     equal to @dstSubpass@ and @srcStageMask@ includes a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stage>,
--     @dstStageMask@ /must/ only contain
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stages>
--
-- -   #VUID-VkSubpassDependency2-srcAccessMask-03088# Any access flag
--     included in @srcAccessMask@ /must/ be supported by one of the
--     pipeline stages in @srcStageMask@, as specified in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-VkSubpassDependency2-dstAccessMask-03089# Any access flag
--     included in @dstAccessMask@ /must/ be supported by one of the
--     pipeline stages in @dstStageMask@, as specified in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-VkSubpassDependency2-dependencyFlags-03090# If
--     @dependencyFlags@ includes
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT',
--     @srcSubpass@ /must/ not be equal to
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'
--
-- -   #VUID-VkSubpassDependency2-dependencyFlags-03091# If
--     @dependencyFlags@ includes
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT',
--     @dstSubpass@ /must/ not be equal to
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'
--
-- -   #VUID-VkSubpassDependency2-srcSubpass-02245# If @srcSubpass@ equals
--     @dstSubpass@, and @srcStageMask@ and @dstStageMask@ both include a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stage>,
--     then @dependencyFlags@ /must/ include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT'
--
-- -   #VUID-VkSubpassDependency2-viewOffset-02530# If @viewOffset@ is not
--     equal to @0@, @srcSubpass@ /must/ not be equal to @dstSubpass@
--
-- -   #VUID-VkSubpassDependency2-dependencyFlags-03092# If
--     @dependencyFlags@ does not include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT',
--     @viewOffset@ /must/ be @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassDependency2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2'
--
-- -   #VUID-VkSubpassDependency2-pNext-pNext# @pNext@ /must/ be @NULL@ or
--     a pointer to a valid instance of
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.MemoryBarrier2'
--
-- -   #VUID-VkSubpassDependency2-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-parameter# @srcStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-parameter# @dstStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkSubpassDependency2-srcAccessMask-parameter# @srcAccessMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' values
--
-- -   #VUID-VkSubpassDependency2-dstAccessMask-parameter# @dstAccessMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' values
--
-- -   #VUID-VkSubpassDependency2-dependencyFlags-parameter#
--     @dependencyFlags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags',
-- 'RenderPassCreateInfo2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubpassDependency2 (es :: [Type]) = SubpassDependency2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @srcSubpass@ is the subpass index of the first subpass in the
    -- dependency, or 'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'.
    srcSubpass :: Word32
  , -- | @dstSubpass@ is the subpass index of the second subpass in the
    -- dependency, or 'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'.
    dstSubpass :: Word32
  , -- | @srcStageMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
    -- specifying the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>.
    srcStageMask :: PipelineStageFlags
  , -- | @dstStageMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
    -- specifying the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
    dstStageMask :: PipelineStageFlags
  , -- | @srcAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
    dstAccessMask :: AccessFlags
  , -- | @dependencyFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits'.
    dependencyFlags :: DependencyFlags
  , -- | @viewOffset@ controls which views in the source subpass the views in the
    -- destination subpass depend on.
    viewOffset :: Int32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDependency2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubpassDependency2 es)

instance Extensible SubpassDependency2 where
  extensibleTypeName = "SubpassDependency2"
  setNext SubpassDependency2{..} next' = SubpassDependency2{next = next', ..}
  getNext SubpassDependency2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubpassDependency2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MemoryBarrier2 = Just f
    | otherwise = Nothing

instance ( Extendss SubpassDependency2 es
         , PokeChain es ) => ToCStruct (SubpassDependency2 es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDependency2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (srcSubpass)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (dstSubpass)
    lift $ poke ((p `plusPtr` 24 :: Ptr PipelineStageFlags)) (srcStageMask)
    lift $ poke ((p `plusPtr` 28 :: Ptr PipelineStageFlags)) (dstStageMask)
    lift $ poke ((p `plusPtr` 32 :: Ptr AccessFlags)) (srcAccessMask)
    lift $ poke ((p `plusPtr` 36 :: Ptr AccessFlags)) (dstAccessMask)
    lift $ poke ((p `plusPtr` 40 :: Ptr DependencyFlags)) (dependencyFlags)
    lift $ poke ((p `plusPtr` 44 :: Ptr Int32)) (viewOffset)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr Int32)) (zero)
    lift $ f

instance ( Extendss SubpassDependency2 es
         , PeekChain es ) => FromCStruct (SubpassDependency2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcSubpass <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    dstSubpass <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    srcStageMask <- peek @PipelineStageFlags ((p `plusPtr` 24 :: Ptr PipelineStageFlags))
    dstStageMask <- peek @PipelineStageFlags ((p `plusPtr` 28 :: Ptr PipelineStageFlags))
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 32 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 36 :: Ptr AccessFlags))
    dependencyFlags <- peek @DependencyFlags ((p `plusPtr` 40 :: Ptr DependencyFlags))
    viewOffset <- peek @Int32 ((p `plusPtr` 44 :: Ptr Int32))
    pure $ SubpassDependency2
             next
             srcSubpass
             dstSubpass
             srcStageMask
             dstStageMask
             srcAccessMask
             dstAccessMask
             dependencyFlags
             viewOffset

instance es ~ '[] => Zero (SubpassDependency2 es) where
  zero = SubpassDependency2
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkRenderPassCreateInfo2 - Structure specifying parameters of a newly
-- created render pass
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Vulkan.Core10.Pass.RenderPassCreateInfo' have the identical effect to
-- those parameters; the child structures are variants of those used in
-- 'Vulkan.Core10.Pass.RenderPassCreateInfo' which add @sType@ and @pNext@
-- parameters, allowing them to be extended.
--
-- If the 'SubpassDescription2'::@viewMask@ member of any element of
-- @pSubpasses@ is not zero, /multiview/ functionality is considered to be
-- enabled for this render pass.
--
-- @correlatedViewMaskCount@ and @pCorrelatedViewMasks@ have the same
-- effect as
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@correlationMaskCount@
-- and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pCorrelationMasks@,
-- respectively.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassCreateInfo2-None-03049# If any two subpasses
--     operate on attachments with overlapping ranges of the same
--     'Vulkan.Core10.Handles.DeviceMemory' object, and at least one
--     subpass writes to that area of 'Vulkan.Core10.Handles.DeviceMemory',
--     a subpass dependency /must/ be included (either directly or via some
--     intermediate subpasses) between them
--
-- -   #VUID-VkRenderPassCreateInfo2-attachment-03050# If the @attachment@
--     member of any element of @pInputAttachments@, @pColorAttachments@,
--     @pResolveAttachments@ or @pDepthStencilAttachment@, or the
--     attachment indexed by any element of @pPreserveAttachments@ in any
--     element of @pSubpasses@ is bound to a range of a
--     'Vulkan.Core10.Handles.DeviceMemory' object that overlaps with any
--     other attachment in any subpass (including the same subpass), the
--     'AttachmentDescription2' structures describing them /must/ include
--     'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
--     in @flags@
--
-- -   #VUID-VkRenderPassCreateInfo2-attachment-03051# If the @attachment@
--     member of any element of @pInputAttachments@, @pColorAttachments@,
--     @pResolveAttachments@ or @pDepthStencilAttachment@, or any element
--     of @pPreserveAttachments@ in any element of @pSubpasses@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then it /must/ be
--     less than @attachmentCount@
--
-- -   #VUID-VkRenderPassCreateInfo2-fragmentDensityMapAttachment-06472# If
--     the pNext chain includes a
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT'
--     structure and the @fragmentDensityMapAttachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then @attachment@
--     /must/ be less than @attachmentCount@
--
-- -   #VUID-VkRenderPassCreateInfo2-pSubpasses-06473# If the @pSubpasses@
--     pNext chain includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
--     structure and the @pDepthStencilResolveAttachment@ member is not
--     @NULL@ and does not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then @attachment@
--     /must/ be less than @attachmentCount@
--
-- -   #VUID-VkRenderPassCreateInfo2-pAttachments-02522# For any member of
--     @pAttachments@ with a @loadOp@ equal to
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderPassCreateInfo2-pAttachments-02523# For any member of
--     @pAttachments@ with a @stencilLoadOp@ equal to
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderPassCreateInfo2-pDependencies-03054# For any element
--     of @pDependencies@, if the @srcSubpass@ is not
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', all stage flags
--     included in the @srcStageMask@ member of that dependency /must/ be a
--     pipeline stage supported by the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass
--
-- -   #VUID-VkRenderPassCreateInfo2-pDependencies-03055# For any element
--     of @pDependencies@, if the @dstSubpass@ is not
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', all stage flags
--     included in the @dstStageMask@ member of that dependency /must/ be a
--     pipeline stage supported by the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the destination
--     subpass
--
-- -   #VUID-VkRenderPassCreateInfo2-pCorrelatedViewMasks-03056# The set of
--     bits included in any element of @pCorrelatedViewMasks@ /must/ not
--     overlap with the set of bits included in any other element of
--     @pCorrelatedViewMasks@
--
-- -   #VUID-VkRenderPassCreateInfo2-viewMask-03057# If the
--     'SubpassDescription2'::@viewMask@ member of all elements of
--     @pSubpasses@ is @0@, @correlatedViewMaskCount@ /must/ be @0@
--
-- -   #VUID-VkRenderPassCreateInfo2-viewMask-03058# The
--     'SubpassDescription2'::@viewMask@ member of all elements of
--     @pSubpasses@ /must/ either all be @0@, or all not be @0@
--
-- -   #VUID-VkRenderPassCreateInfo2-viewMask-03059# If the
--     'SubpassDescription2'::@viewMask@ member of all elements of
--     @pSubpasses@ is @0@, the @dependencyFlags@ member of any element of
--     @pDependencies@ /must/ not include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   #VUID-VkRenderPassCreateInfo2-pDependencies-03060# For any element
--     of @pDependencies@ where its @srcSubpass@ member equals its
--     @dstSubpass@ member, if the @viewMask@ member of the corresponding
--     element of @pSubpasses@ includes more than one bit, its
--     @dependencyFlags@ member /must/ include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   #VUID-VkRenderPassCreateInfo2-attachment-02525# If the @attachment@
--     member of any element of the @pInputAttachments@ member of any
--     element of @pSubpasses@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the @aspectMask@
--     member of that element of @pInputAttachments@ /must/ only include
--     aspects that are present in images of the format specified by the
--     element of @pAttachments@ specified by @attachment@
--
-- -   #VUID-VkRenderPassCreateInfo2-srcSubpass-02526# The @srcSubpass@
--     member of each element of @pDependencies@ /must/ be less than
--     @subpassCount@
--
-- -   #VUID-VkRenderPassCreateInfo2-dstSubpass-02527# The @dstSubpass@
--     member of each element of @pDependencies@ /must/ be less than
--     @subpassCount@
--
-- -   #VUID-VkRenderPassCreateInfo2-pAttachments-04585# If any element of
--     @pAttachments@ is used as a fragment shading rate attachment in any
--     subpass, it /must/ not be used as any other attachment in the render
--     pass
--
-- -   #VUID-VkRenderPassCreateInfo2-pAttachments-09387# If any element of
--     @pAttachments@ is used as a fragment shading rate attachment, the
--     @loadOp@ for that attachment /must/ not be
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   #VUID-VkRenderPassCreateInfo2-flags-04521# If @flags@ includes
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM',
--     an element of @pSubpasses@ includes an instance of
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'
--     in its @pNext@ chain, and the @pFragmentShadingRateAttachment@
--     member of that structure is not equal to @NULL@, the @attachment@
--     member of @pFragmentShadingRateAttachment@ /must/ be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderPassCreateInfo2-pAttachments-04586# If any element of
--     @pAttachments@ is used as a fragment shading rate attachment in any
--     subpass, it /must/ have an image format whose
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkRenderPassCreateInfo2-rasterizationSamples-04905# If the
--     pipeline is being created with fragment shader state, and the
--     @VK_QCOM_render_pass_shader_resolve extension@ is enabled, and if
--     subpass has any input attachments, and if the subpass description
--     contains
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM',
--     then the sample count of the input attachments /must/ equal
--     @rasterizationSamples@
--
-- -   #VUID-VkRenderPassCreateInfo2-sampleShadingEnable-04906# If the
--     pipeline is being created with fragment shader state, and the
--     @VK_QCOM_render_pass_shader_resolve@ extension is enabled, and if
--     the subpass description contains
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM',
--     then @sampleShadingEnable@ /must/ be false
--
-- -   #VUID-VkRenderPassCreateInfo2-flags-04907# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM',
--     and if @pResolveAttachments@ is not @NULL@, then each resolve
--     attachment /must/ be 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderPassCreateInfo2-flags-04908# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM',
--     and if @pDepthStencilResolveAttachment@ is not @NULL@, then the
--     depth\/stencil resolve attachment /must/ be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderPassCreateInfo2-flags-04909# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM',
--     then the subpass /must/ be the last subpass in a subpass dependency
--     chain
--
-- -   #VUID-VkRenderPassCreateInfo2-attachment-06244# If the @attachment@
--     member of the @pDepthStencilAttachment@ member of an element of
--     @pSubpasses@ is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     the @layout@ member of that same structure is either
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     and the @pNext@ chain of that structure does not include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout'
--     structure, then the element of @pAttachments@ with an index equal to
--     @attachment@ /must/ not have a @format@ that includes both depth and
--     stencil components
--
-- -   #VUID-VkRenderPassCreateInfo2-attachment-06245# If the @attachment@
--     member of the @pDepthStencilAttachment@ member of an element of
--     @pSubpasses@ is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--     and the @layout@ member of that same structure is either
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     then the element of @pAttachments@ with an index equal to
--     @attachment@ /must/ have a @format@ that includes only a stencil
--     component
--
-- -   #VUID-VkRenderPassCreateInfo2-attachment-06246# If the @attachment@
--     member of the @pDepthStencilAttachment@ member of an element of
--     @pSubpasses@ is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--     and the @layout@ member of that same structure is either
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     then the element of @pAttachments@ with an index equal to
--     @attachment@ /must/ not have a @format@ that includes only a stencil
--     component
--
-- -   #VUID-VkRenderPassCreateInfo2-pResolveAttachments-09331# If any
--     element of @pResolveAttachments@ of any element of @pSubpasses@
--     references an attachment description with a format of
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT'::@fragmentDensityMapAttachment->attachment@
--     /must/ be 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassCreateInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2'
--
-- -   #VUID-VkRenderPassCreateInfo2-pNext-pNext# Each @pNext@ member of
--     any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_subpass_merge_feedback.RenderPassCreationControlEXT',
--     'Vulkan.Extensions.VK_EXT_subpass_merge_feedback.RenderPassCreationFeedbackCreateInfoEXT',
--     or
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT'
--
-- -   #VUID-VkRenderPassCreateInfo2-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkRenderPassCreateInfo2-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlagBits'
--     values
--
-- -   #VUID-VkRenderPassCreateInfo2-pAttachments-parameter# If
--     @attachmentCount@ is not @0@, @pAttachments@ /must/ be a valid
--     pointer to an array of @attachmentCount@ valid
--     'AttachmentDescription2' structures
--
-- -   #VUID-VkRenderPassCreateInfo2-pSubpasses-parameter# @pSubpasses@
--     /must/ be a valid pointer to an array of @subpassCount@ valid
--     'SubpassDescription2' structures
--
-- -   #VUID-VkRenderPassCreateInfo2-pDependencies-parameter# If
--     @dependencyCount@ is not @0@, @pDependencies@ /must/ be a valid
--     pointer to an array of @dependencyCount@ valid 'SubpassDependency2'
--     structures
--
-- -   #VUID-VkRenderPassCreateInfo2-pCorrelatedViewMasks-parameter# If
--     @correlatedViewMaskCount@ is not @0@, @pCorrelatedViewMasks@ /must/
--     be a valid pointer to an array of @correlatedViewMaskCount@
--     @uint32_t@ values
--
-- -   #VUID-VkRenderPassCreateInfo2-subpassCount-arraylength#
--     @subpassCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'AttachmentDescription2',
-- 'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'SubpassDependency2',
-- 'SubpassDescription2', 'createRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.createRenderPass2KHR'
data RenderPassCreateInfo2 (es :: [Type]) = RenderPassCreateInfo2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: RenderPassCreateFlags
  , -- | @pAttachments@ is a pointer to an array of @attachmentCount@
    -- 'AttachmentDescription2' structures describing the attachments used by
    -- the render pass.
    attachments :: Vector (SomeStruct AttachmentDescription2)
  , -- | @pSubpasses@ is a pointer to an array of @subpassCount@
    -- 'SubpassDescription2' structures describing each subpass.
    subpasses :: Vector (SomeStruct SubpassDescription2)
  , -- | @pDependencies@ is a pointer to an array of @dependencyCount@
    -- 'SubpassDependency2' structures describing dependencies between pairs of
    -- subpasses.
    dependencies :: Vector (SomeStruct SubpassDependency2)
  , -- | @pCorrelatedViewMasks@ is a pointer to an array of view masks indicating
    -- sets of views that /may/ be more efficient to render concurrently.
    correlatedViewMasks :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassCreateInfo2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderPassCreateInfo2 es)

instance Extensible RenderPassCreateInfo2 where
  extensibleTypeName = "RenderPassCreateInfo2"
  setNext RenderPassCreateInfo2{..} next' = RenderPassCreateInfo2{next = next', ..}
  getNext RenderPassCreateInfo2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderPassCreateInfo2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassCreationFeedbackCreateInfoEXT = Just f
    | Just Refl <- eqT @e @RenderPassCreationControlEXT = Just f
    | Just Refl <- eqT @e @RenderPassFragmentDensityMapCreateInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss RenderPassCreateInfo2 es
         , PokeChain es ) => ToCStruct (RenderPassCreateInfo2 es) where
  withCStruct x f = allocaBytes 80 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassCreateInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPassCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32))
    pPAttachments' <- ContT $ allocaBytes @(AttachmentDescription2 _) ((Data.Vector.length (attachments)) * 56)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPAttachments' `plusPtr` (56 * (i)) :: Ptr (AttachmentDescription2 _))) (e) . ($ ())) (attachments)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentDescription2 _)))) (pPAttachments')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (subpasses)) :: Word32))
    pPSubpasses' <- ContT $ allocaBytes @(SubpassDescription2 _) ((Data.Vector.length (subpasses)) * 88)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSubpasses' `plusPtr` (88 * (i)) :: Ptr (SubpassDescription2 _))) (e) . ($ ())) (subpasses)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (SubpassDescription2 _)))) (pPSubpasses')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (dependencies)) :: Word32))
    pPDependencies' <- ContT $ allocaBytes @(SubpassDependency2 _) ((Data.Vector.length (dependencies)) * 48)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPDependencies' `plusPtr` (48 * (i)) :: Ptr (SubpassDependency2 _))) (e) . ($ ())) (dependencies)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (SubpassDependency2 _)))) (pPDependencies')
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (correlatedViewMasks)) :: Word32))
    pPCorrelatedViewMasks' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (correlatedViewMasks)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCorrelatedViewMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (correlatedViewMasks)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPCorrelatedViewMasks')
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss RenderPassCreateInfo2 es
         , PeekChain es ) => FromCStruct (RenderPassCreateInfo2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @RenderPassCreateFlags ((p `plusPtr` 16 :: Ptr RenderPassCreateFlags))
    attachmentCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pAttachments <- peek @(Ptr (AttachmentDescription2 _)) ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentDescription2 _))))
    pAttachments' <- generateM (fromIntegral attachmentCount) (\i -> peekSomeCStruct (forgetExtensions ((pAttachments `advancePtrBytes` (56 * (i)) :: Ptr (AttachmentDescription2 _)))))
    subpassCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSubpasses <- peek @(Ptr (SubpassDescription2 _)) ((p `plusPtr` 40 :: Ptr (Ptr (SubpassDescription2 _))))
    pSubpasses' <- generateM (fromIntegral subpassCount) (\i -> peekSomeCStruct (forgetExtensions ((pSubpasses `advancePtrBytes` (88 * (i)) :: Ptr (SubpassDescription2 _)))))
    dependencyCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pDependencies <- peek @(Ptr (SubpassDependency2 _)) ((p `plusPtr` 56 :: Ptr (Ptr (SubpassDependency2 _))))
    pDependencies' <- generateM (fromIntegral dependencyCount) (\i -> peekSomeCStruct (forgetExtensions ((pDependencies `advancePtrBytes` (48 * (i)) :: Ptr (SubpassDependency2 _)))))
    correlatedViewMaskCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pCorrelatedViewMasks <- peek @(Ptr Word32) ((p `plusPtr` 72 :: Ptr (Ptr Word32)))
    pCorrelatedViewMasks' <- generateM (fromIntegral correlatedViewMaskCount) (\i -> peek @Word32 ((pCorrelatedViewMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ RenderPassCreateInfo2
             next
             flags
             pAttachments'
             pSubpasses'
             pDependencies'
             pCorrelatedViewMasks'

instance es ~ '[] => Zero (RenderPassCreateInfo2 es) where
  zero = RenderPassCreateInfo2
           ()
           zero
           mempty
           mempty
           mempty
           mempty


-- | VkSubpassBeginInfo - Structure specifying subpass begin information
--
-- == Valid Usage
--
-- -   #VUID-VkSubpassBeginInfo-contents-09382# If @contents@ is
--     'Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT',
--     then
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nestedCommandBuffer nestedCommandBuffer>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassBeginInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_BEGIN_INFO'
--
-- -   #VUID-VkSubpassBeginInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkSubpassBeginInfo-contents-parameter# @contents@ /must/ be a
--     valid 'Vulkan.Core10.Enums.SubpassContents.SubpassContents' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Enums.SubpassContents.SubpassContents',
-- 'cmdBeginRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdBeginRenderPass2KHR',
-- 'cmdNextSubpass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdNextSubpass2KHR'
data SubpassBeginInfo = SubpassBeginInfo
  { -- | @contents@ is a 'Vulkan.Core10.Enums.SubpassContents.SubpassContents'
    -- value specifying how the commands in the next subpass will be provided.
    contents :: SubpassContents }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassBeginInfo)
#endif
deriving instance Show SubpassBeginInfo

instance ToCStruct SubpassBeginInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassBeginInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SubpassContents)) (contents)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SubpassContents)) (zero)
    f

instance FromCStruct SubpassBeginInfo where
  peekCStruct p = do
    contents <- peek @SubpassContents ((p `plusPtr` 16 :: Ptr SubpassContents))
    pure $ SubpassBeginInfo
             contents

instance Storable SubpassBeginInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassBeginInfo where
  zero = SubpassBeginInfo
           zero


-- | VkSubpassEndInfo - Structure specifying subpass end information
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassEndInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_END_INFO'
--
-- -   #VUID-VkSubpassEndInfo-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_QCOM_fragment_density_map_offset.SubpassFragmentDensityMapOffsetEndInfoQCOM'
--
-- -   #VUID-VkSubpassEndInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdEndRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdEndRenderPass2KHR',
-- 'cmdNextSubpass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdNextSubpass2KHR'
data SubpassEndInfo (es :: [Type]) = SubpassEndInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassEndInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubpassEndInfo es)

instance Extensible SubpassEndInfo where
  extensibleTypeName = "SubpassEndInfo"
  setNext _ next' = SubpassEndInfo{next = next'}
  getNext SubpassEndInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubpassEndInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SubpassFragmentDensityMapOffsetEndInfoQCOM = Just f
    | otherwise = Nothing

instance ( Extendss SubpassEndInfo es
         , PokeChain es ) => ToCStruct (SubpassEndInfo es) where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassEndInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_END_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_END_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss SubpassEndInfo es
         , PeekChain es ) => FromCStruct (SubpassEndInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    pure $ SubpassEndInfo
             next

instance es ~ '[] => Zero (SubpassEndInfo es) where
  zero = SubpassEndInfo
           ()

