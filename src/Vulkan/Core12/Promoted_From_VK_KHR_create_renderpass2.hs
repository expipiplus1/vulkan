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
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginRenderPass2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndRenderPass2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdNextSubpass2))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRenderPass2))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateAttachmentInfoKHR)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (RenderPassFragmentDensityMapCreateInfoEXT)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (SubpassDescriptionDepthStencilResolve)
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlags)
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
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                     -- chapter.
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (RenderPass)
createRenderPass2 device createInfo allocator = liftIO . evalContT $ do
  let vkCreateRenderPass2Ptr = pVkCreateRenderPass2 (deviceCmds (device :: Device))
  lift $ unless (vkCreateRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateRenderPass2 is null" Nothing Nothing
  let vkCreateRenderPass2' = mkVkCreateRenderPass2 vkCreateRenderPass2Ptr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPRenderPass <- ContT $ bracket (callocBytes @RenderPass 8) free
  r <- lift $ traceAroundEvent "vkCreateRenderPass2" (vkCreateRenderPass2' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPRenderPass))
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
-- -   #VUID-vkCmdBeginRenderPass2-initialLayout-03100# If any of the
--     @initialLayout@ members of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures specified when
--     creating the render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is not
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED', then each
--     such @initialLayout@ /must/ be equal to the current layout of the
--     corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@
--
-- -   #VUID-vkCmdBeginRenderPass2-srcStageMask-03101# The @srcStageMask@
--     and @dstStageMask@ members of any element of the @pDependencies@
--     member of 'Vulkan.Core10.Pass.RenderPassCreateInfo' used to create
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
cmdBeginRenderPass2 commandBuffer renderPassBegin subpassBeginInfo = liftIO . evalContT $ do
  let vkCmdBeginRenderPass2Ptr = pVkCmdBeginRenderPass2 (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginRenderPass2 is null" Nothing Nothing
  let vkCmdBeginRenderPass2' = mkVkCmdBeginRenderPass2 vkCmdBeginRenderPass2Ptr
  pRenderPassBegin <- ContT $ withCStruct (renderPassBegin)
  pSubpassBeginInfo <- ContT $ withCStruct (subpassBeginInfo)
  lift $ traceAroundEvent "vkCmdBeginRenderPass2" (vkCmdBeginRenderPass2' (commandBufferHandle (commandBuffer)) (forgetExtensions pRenderPassBegin) pSubpassBeginInfo)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginRenderPass2' and 'cmdEndRenderPass2'
--
-- Note that 'cmdEndRenderPass2' is *not* called if an exception is thrown
-- by the inner action.
cmdUseRenderPass2 :: forall a io r . (Extendss RenderPassBeginInfo a, PokeChain a, MonadIO io) => CommandBuffer -> RenderPassBeginInfo a -> SubpassBeginInfo -> SubpassEndInfo -> io r -> io r
cmdUseRenderPass2 commandBuffer pRenderPassBegin pSubpassBeginInfo pSubpassEndInfo a =
  (cmdBeginRenderPass2 commandBuffer pRenderPassBegin pSubpassBeginInfo) *> a <* (cmdEndRenderPass2 commandBuffer pSubpassEndInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr SubpassBeginInfo -> Ptr SubpassEndInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr SubpassBeginInfo -> Ptr SubpassEndInfo -> IO ()

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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'SubpassBeginInfo',
-- 'SubpassEndInfo'
cmdNextSubpass2 :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer in which to record the command.
                   CommandBuffer
                -> -- | @pSubpassBeginInfo@ is a pointer to a 'SubpassBeginInfo' structure
                   -- containing information about the subpass which is about to begin
                   -- rendering.
                   SubpassBeginInfo
                -> -- | @pSubpassEndInfo@ is a pointer to a 'SubpassEndInfo' structure
                   -- containing information about how the previous subpass will be ended.
                   SubpassEndInfo
                -> io ()
cmdNextSubpass2 commandBuffer subpassBeginInfo subpassEndInfo = liftIO . evalContT $ do
  let vkCmdNextSubpass2Ptr = pVkCmdNextSubpass2 (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdNextSubpass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdNextSubpass2 is null" Nothing Nothing
  let vkCmdNextSubpass2' = mkVkCmdNextSubpass2 vkCmdNextSubpass2Ptr
  pSubpassBeginInfo <- ContT $ withCStruct (subpassBeginInfo)
  pSubpassEndInfo <- ContT $ withCStruct (subpassEndInfo)
  lift $ traceAroundEvent "vkCmdNextSubpass2" (vkCmdNextSubpass2' (commandBufferHandle (commandBuffer)) pSubpassBeginInfo pSubpassEndInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr SubpassEndInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr SubpassEndInfo -> IO ()

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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'SubpassEndInfo'
cmdEndRenderPass2 :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer in which to end the current render
                     -- pass instance.
                     CommandBuffer
                  -> -- | @pSubpassEndInfo@ is a pointer to a 'SubpassEndInfo' structure
                     -- containing information about how the previous subpass will be ended.
                     SubpassEndInfo
                  -> io ()
cmdEndRenderPass2 commandBuffer subpassEndInfo = liftIO . evalContT $ do
  let vkCmdEndRenderPass2Ptr = pVkCmdEndRenderPass2 (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdEndRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndRenderPass2 is null" Nothing Nothing
  let vkCmdEndRenderPass2' = mkVkCmdEndRenderPass2 vkCmdEndRenderPass2Ptr
  pSubpassEndInfo <- ContT $ withCStruct (subpassEndInfo)
  lift $ traceAroundEvent "vkCmdEndRenderPass2" (vkCmdEndRenderPass2' (commandBufferHandle (commandBuffer)) pSubpassEndInfo)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
-- feature is enabled, and @format@ is a depth\/stencil format,
-- @initialLayout@ and @finalLayout@ /can/ be set to a layout that only
-- specifies the layout of the depth aspect.
--
-- If @format@ is a depth\/stencil format, and @initialLayout@ only
-- specifies the initial layout of the depth aspect of the attachment, the
-- initial layout of the stencil aspect is specified by the
-- @stencilInitialLayout@ member of a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
-- structure included in the @pNext@ chain. Otherwise, @initialLayout@
-- describes the initial layout for all relevant image aspects.
--
-- If @format@ is a depth\/stencil format, and @finalLayout@ only specifies
-- the final layout of the depth aspect of the attachment, the final layout
-- of the stencil aspect is specified by the @stencilFinalLayout@ member of
-- a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
-- structure included in the @pNext@ chain. Otherwise, @finalLayout@
-- describes the final layout for all relevant image aspects.
--
-- == Valid Usage
--
-- -   #VUID-VkAttachmentDescription2-finalLayout-03061# @finalLayout@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkAttachmentDescription2-format-03294# If @format@ is a color
--     format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03295# If @format@ is a
--     depth\/stencil format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03296# If @format@ is a color
--     format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03297# If @format@ is a
--     depth\/stencil format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-separateDepthStencilLayouts-03298# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-separateDepthStencilLayouts-03299# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03300# If @format@ is a color
--     format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03301# If @format@ is a color
--     format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03302# If @format@ is a
--     depth\/stencil format which includes both depth and stencil aspects,
--     and @initialLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structure
--
-- -   #VUID-VkAttachmentDescription2-format-03303# If @format@ is a
--     depth\/stencil format which includes both depth and stencil aspects,
--     and @finalLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structure
--
-- -   #VUID-VkAttachmentDescription2-format-03304# If @format@ is a
--     depth\/stencil format which includes only the depth aspect,
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03305# If @format@ is a
--     depth\/stencil format which includes only the depth aspect,
--     @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03306# If @format@ is a
--     depth\/stencil format which includes only the stencil aspect,
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription2-format-03307# If @format@ is a
--     depth\/stencil format which includes only the stencil aspect,
--     @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAttachmentDescription2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2'
--
-- -   #VUID-VkAttachmentDescription2-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
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
  , -- | @samples@ is the number of samples of the image as defined in
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'.
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
  setNext x next = x{next = next}
  getNext AttachmentDescription2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AttachmentDescription2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @AttachmentDescriptionStencilLayout = Just f
    | otherwise = Nothing

instance (Extendss AttachmentDescription2 es, PokeChain es) => ToCStruct (AttachmentDescription2 es) where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
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

instance (Extendss AttachmentDescription2 es, PeekChain es) => FromCStruct (AttachmentDescription2 es) where
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
             next flags format samples loadOp storeOp stencilLoadOp stencilStoreOp initialLayout finalLayout

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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
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
    -- corresponding index in
    -- 'Vulkan.Core10.Pass.RenderPassCreateInfo'::@pAttachments@, or
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
  setNext x next = x{next = next}
  getNext AttachmentReference2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AttachmentReference2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @AttachmentReferenceStencilLayout = Just f
    | otherwise = Nothing

instance (Extendss AttachmentReference2 es, PokeChain es) => ToCStruct (AttachmentReference2 es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
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

instance (Extendss AttachmentReference2 es, PeekChain es) => FromCStruct (AttachmentReference2 es) where
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
-- If an instance of
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'
-- is included in the @pNext@ chain, @pFragmentShadingRateAttachment@ is
-- not @NULL@, and its @attachment@ member is not
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the identified
-- attachment defines a fragment shading rate attachment for that subpass.
--
-- == Valid Usage
--
-- -   #VUID-VkSubpassDescription2-pipelineBindPoint-03062#
--     @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
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
-- -   #VUID-VkSubpassDescription2-pResolveAttachments-03065# If
--     @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that does not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the corresponding
--     color attachment /must/ not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-pResolveAttachments-03066# If
--     @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the
--     corresponding color attachment /must/ not have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkSubpassDescription2-pResolveAttachments-03067# If
--     @pResolveAttachments@ is not @NULL@, each resolve attachment that is
--     not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have a
--     sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkSubpassDescription2-pResolveAttachments-03068# Any given
--     element of @pResolveAttachments@ /must/ have the same
--     'Vulkan.Core10.Enums.Format.Format' as its corresponding color
--     attachment
--
-- -   #VUID-VkSubpassDescription2-pColorAttachments-03069# All attachments
--     in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have the same
--     sample count
--
-- -   #VUID-VkSubpassDescription2-pInputAttachments-02897# All attachments
--     in @pInputAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain at least
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription2-pColorAttachments-02898# All attachments
--     in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription2-pResolveAttachments-02899# All
--     attachments in @pResolveAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription2-pDepthStencilAttachment-02900# If
--     @pDepthStencilAttachment@ is not @NULL@ and the attachment is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' then it /must/ have a
--     image format whose
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription2-pColorAttachments-03070# If the
--     @VK_AMD_mixed_attachment_samples@ extension is enabled, all
--     attachments in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have a sample
--     count that is smaller than or equal to the sample count of
--     @pDepthStencilAttachment@ if it is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-pDepthStencilAttachment-03071# If
--     neither the @VK_AMD_mixed_attachment_samples@ nor the
--     @VK_NV_framebuffer_mixed_samples@ extensions are enabled, and if
--     @pDepthStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and any attachments
--     in @pColorAttachments@ are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', they /must/ have the
--     same sample count
--
-- -   #VUID-VkSubpassDescription2-attachment-03073# The @attachment@
--     member of any element of @pPreserveAttachments@ /must/ not be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription2-pPreserveAttachments-03074# Any given
--     element of @pPreserveAttachments@ /must/ not also be an element of
--     any other member of the subpass description
--
-- -   #VUID-VkSubpassDescription2-layout-02528# If any attachment is used
--     by more than one 'Vulkan.Core10.Pass.AttachmentReference' member,
--     then each use /must/ use the same @layout@
--
-- -   #VUID-VkSubpassDescription2-None-04439# Attachments /must/ follow
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#attachment-type-imagelayout image layout requirements>
--     based on the type of attachment it is being used as
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
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index @i@
--
-- -   #VUID-VkSubpassDescription2-pDepthStencilAttachment-04440# An
--     attachment /must/ not be used in both @pDepthStencilAttachment@ and
--     @pColorAttachments@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassDescription2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2'
--
-- -   #VUID-VkSubpassDescription2-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'
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
  , -- | @pColorAttachments@ is a pointer to an array of 'AttachmentReference2'
    -- structures defining the color attachments for this subpass and their
    -- layouts.
    colorAttachments :: Vector (SomeStruct AttachmentReference2)
  , -- | @pResolveAttachments@ is an optional array of @colorAttachmentCount@
    -- 'AttachmentReference2' structures defining the resolve attachments for
    -- this subpass and their layouts.
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
  setNext x next = x{next = next}
  getNext SubpassDescription2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubpassDescription2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @FragmentShadingRateAttachmentInfoKHR = Just f
    | Just Refl <- eqT @e @SubpassDescriptionDepthStencilResolve = Just f
    | otherwise = Nothing

instance (Extendss SubpassDescription2 es, PokeChain es) => ToCStruct (SubpassDescription2 es) where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDescription2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SubpassDescriptionFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (inputAttachments)) :: Word32))
    pPInputAttachments' <- ContT $ allocaBytesAligned @(AttachmentReference2 _) ((Data.Vector.length (inputAttachments)) * 32) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPInputAttachments' `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) (inputAttachments)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (AttachmentReference2 _)))) (pPInputAttachments')
    let pColorAttachmentsLength = Data.Vector.length $ (colorAttachments)
    let pResolveAttachmentsLength = Data.Vector.length $ (resolveAttachments)
    lift $ unless (fromIntegral pResolveAttachmentsLength == pColorAttachmentsLength || pResolveAttachmentsLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pResolveAttachments and pColorAttachments must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral pColorAttachmentsLength :: Word32))
    pPColorAttachments' <- ContT $ allocaBytesAligned @(AttachmentReference2 _) ((Data.Vector.length (colorAttachments)) * 32) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPColorAttachments' `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) (colorAttachments)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (AttachmentReference2 _)))) (pPColorAttachments')
    pResolveAttachments'' <- if Data.Vector.null (resolveAttachments)
      then pure nullPtr
      else do
        pPResolveAttachments <- ContT $ allocaBytesAligned @(AttachmentReference2 _) (((Data.Vector.length (resolveAttachments))) * 32) 8
        Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPResolveAttachments `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) ((resolveAttachments))
        pure $ pPResolveAttachments
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (AttachmentReference2 _)))) pResolveAttachments''
    pDepthStencilAttachment'' <- case (depthStencilAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (AttachmentReference2 _)))) pDepthStencilAttachment''
    lift $ poke ((p `plusPtr` 72 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (preserveAttachments)) :: Word32))
    pPPreserveAttachments' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (preserveAttachments)) * 4) 4
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

instance (Extendss SubpassDescription2 es, PeekChain es) => FromCStruct (SubpassDescription2 es) where
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
             next flags pipelineBindPoint viewMask pInputAttachments' pColorAttachments' pResolveAttachments' pDepthStencilAttachment' pPreserveAttachments'

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
-- == Valid Usage
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-03080# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-03081# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-03082# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-03083# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
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
-- -   #VUID-VkSubpassDependency2-srcSubpass-03087# If @srcSubpass@ is
--     equal to @dstSubpass@ and not all of the stages in @srcStageMask@
--     and @dstStageMask@ are
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stages>,
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
--     pipeline stage in @srcStageMask@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
--     than or equal to the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earliest>
--     pipeline stage in @dstStageMask@
--
-- -   #VUID-VkSubpassDependency2-srcAccessMask-03088# Any access flag
--     included in @srcAccessMask@ /must/ be supported by one of the
--     pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-VkSubpassDependency2-dstAccessMask-03089# Any access flag
--     included in @dstAccessMask@ /must/ be supported by one of the
--     pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stage>,
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
-- -   #VUID-VkSubpassDependency2-srcStageMask-02103# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-02104# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-02105# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-02106# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassDependency2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2'
--
-- -   #VUID-VkSubpassDependency2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-parameter# @srcStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkSubpassDependency2-srcStageMask-requiredbitmask#
--     @srcStageMask@ /must/ not be @0@
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-parameter# @dstStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkSubpassDependency2-dstStageMask-requiredbitmask#
--     @dstStageMask@ /must/ not be @0@
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
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags',
-- 'RenderPassCreateInfo2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubpassDependency2 = SubpassDependency2
  { -- | @srcSubpass@ is the subpass index of the first subpass in the
    -- dependency, or 'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'.
    srcSubpass :: Word32
  , -- | @dstSubpass@ is the subpass index of the second subpass in the
    -- dependency, or 'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'.
    dstSubpass :: Word32
  , -- | @srcStageMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
    -- specifying the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>.
    srcStageMask :: PipelineStageFlags
  , -- | @dstStageMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
    -- specifying the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
    dstStageMask :: PipelineStageFlags
  , -- | @srcAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
    dstAccessMask :: AccessFlags
  , -- | @dependencyFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits'.
    dependencyFlags :: DependencyFlags
  , -- | @viewOffset@ controls which views in the source subpass the views in the
    -- destination subpass depend on.
    viewOffset :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDependency2)
#endif
deriving instance Show SubpassDependency2

instance ToCStruct SubpassDependency2 where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDependency2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (srcSubpass)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (dstSubpass)
    poke ((p `plusPtr` 24 :: Ptr PipelineStageFlags)) (srcStageMask)
    poke ((p `plusPtr` 28 :: Ptr PipelineStageFlags)) (dstStageMask)
    poke ((p `plusPtr` 32 :: Ptr AccessFlags)) (srcAccessMask)
    poke ((p `plusPtr` 36 :: Ptr AccessFlags)) (dstAccessMask)
    poke ((p `plusPtr` 40 :: Ptr DependencyFlags)) (dependencyFlags)
    poke ((p `plusPtr` 44 :: Ptr Int32)) (viewOffset)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PipelineStageFlags)) (zero)
    poke ((p `plusPtr` 28 :: Ptr PipelineStageFlags)) (zero)
    f

instance FromCStruct SubpassDependency2 where
  peekCStruct p = do
    srcSubpass <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    dstSubpass <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    srcStageMask <- peek @PipelineStageFlags ((p `plusPtr` 24 :: Ptr PipelineStageFlags))
    dstStageMask <- peek @PipelineStageFlags ((p `plusPtr` 28 :: Ptr PipelineStageFlags))
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 32 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 36 :: Ptr AccessFlags))
    dependencyFlags <- peek @DependencyFlags ((p `plusPtr` 40 :: Ptr DependencyFlags))
    viewOffset <- peek @Int32 ((p `plusPtr` 44 :: Ptr Int32))
    pure $ SubpassDependency2
             srcSubpass dstSubpass srcStageMask dstStageMask srcAccessMask dstAccessMask dependencyFlags viewOffset

instance Storable SubpassDependency2 where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassDependency2 where
  zero = SubpassDependency2
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
--     given element of @pSubpasses@ is bound to a range of a
--     'Vulkan.Core10.Handles.DeviceMemory' object that overlaps with any
--     other attachment in any subpass (including the same subpass), the
--     'AttachmentDescription2' structures describing them /must/ include
--     'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
--     in @flags@
--
-- -   #VUID-VkRenderPassCreateInfo2-attachment-03051# If the @attachment@
--     member of any element of @pInputAttachments@, @pColorAttachments@,
--     @pResolveAttachments@ or @pDepthStencilAttachment@, or any element
--     of @pPreserveAttachments@ in any given element of @pSubpasses@ is
--     not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', it /must/ be
--     less than @attachmentCount@
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass
--
-- -   #VUID-VkRenderPassCreateInfo2-pDependencies-03055# For any element
--     of @pDependencies@, if the @dstSubpass@ is not
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', all stage flags
--     included in the @dstStageMask@ member of that dependency /must/ be a
--     pipeline stage supported by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassCreateInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2'
--
-- -   #VUID-VkRenderPassCreateInfo2-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
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
    dependencies :: Vector SubpassDependency2
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
  setNext x next = x{next = next}
  getNext RenderPassCreateInfo2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderPassCreateInfo2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassFragmentDensityMapCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss RenderPassCreateInfo2 es, PokeChain es) => ToCStruct (RenderPassCreateInfo2 es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassCreateInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPassCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32))
    pPAttachments' <- ContT $ allocaBytesAligned @(AttachmentDescription2 _) ((Data.Vector.length (attachments)) * 56) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPAttachments' `plusPtr` (56 * (i)) :: Ptr (AttachmentDescription2 _))) (e) . ($ ())) (attachments)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentDescription2 _)))) (pPAttachments')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (subpasses)) :: Word32))
    pPSubpasses' <- ContT $ allocaBytesAligned @(SubpassDescription2 _) ((Data.Vector.length (subpasses)) * 88) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSubpasses' `plusPtr` (88 * (i)) :: Ptr (SubpassDescription2 _))) (e) . ($ ())) (subpasses)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (SubpassDescription2 _)))) (pPSubpasses')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (dependencies)) :: Word32))
    pPDependencies' <- ContT $ allocaBytesAligned @SubpassDependency2 ((Data.Vector.length (dependencies)) * 48) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDependencies' `plusPtr` (48 * (i)) :: Ptr SubpassDependency2) (e)) (dependencies)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SubpassDependency2))) (pPDependencies')
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (correlatedViewMasks)) :: Word32))
    pPCorrelatedViewMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (correlatedViewMasks)) * 4) 4
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

instance (Extendss RenderPassCreateInfo2 es, PeekChain es) => FromCStruct (RenderPassCreateInfo2 es) where
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
    pDependencies <- peek @(Ptr SubpassDependency2) ((p `plusPtr` 56 :: Ptr (Ptr SubpassDependency2)))
    pDependencies' <- generateM (fromIntegral dependencyCount) (\i -> peekCStruct @SubpassDependency2 ((pDependencies `advancePtrBytes` (48 * (i)) :: Ptr SubpassDependency2)))
    correlatedViewMaskCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pCorrelatedViewMasks <- peek @(Ptr Word32) ((p `plusPtr` 72 :: Ptr (Ptr Word32)))
    pCorrelatedViewMasks' <- generateM (fromIntegral correlatedViewMaskCount) (\i -> peek @Word32 ((pCorrelatedViewMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ RenderPassCreateInfo2
             next flags pAttachments' pSubpasses' pDependencies' pCorrelatedViewMasks'

instance es ~ '[] => Zero (RenderPassCreateInfo2 es) where
  zero = RenderPassCreateInfo2
           ()
           zero
           mempty
           mempty
           mempty
           mempty


-- | VkSubpassBeginInfo - Structure specifying subpass begin info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Enums.SubpassContents.SubpassContents',
-- 'cmdBeginRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdBeginRenderPass2KHR',
-- 'cmdNextSubpass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdNextSubpass2KHR'
data SubpassBeginInfo = SubpassBeginInfo
  { -- | @contents@ is a 'Vulkan.Core10.Enums.SubpassContents.SubpassContents'
    -- value specifying how the commands in the next subpass will be provided.
    --
    -- #VUID-VkSubpassBeginInfo-contents-parameter# @contents@ /must/ be a
    -- valid 'Vulkan.Core10.Enums.SubpassContents.SubpassContents' value
    contents :: SubpassContents }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassBeginInfo)
#endif
deriving instance Show SubpassBeginInfo

instance ToCStruct SubpassBeginInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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


-- | VkSubpassEndInfo - Structure specifying subpass end info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdEndRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdEndRenderPass2KHR',
-- 'cmdNextSubpass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdNextSubpass2KHR'
data SubpassEndInfo = SubpassEndInfo
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassEndInfo)
#endif
deriving instance Show SubpassEndInfo

instance ToCStruct SubpassEndInfo where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassEndInfo f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_END_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_END_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SubpassEndInfo where
  peekCStruct _ = pure $ SubpassEndInfo
                           

instance Storable SubpassEndInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassEndInfo where
  zero = SubpassEndInfo
           

