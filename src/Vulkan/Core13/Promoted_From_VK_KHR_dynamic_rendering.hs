{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_dynamic_rendering"
module Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering  ( cmdBeginRendering
                                                             , cmdUseRendering
                                                             , cmdEndRendering
                                                             , PipelineRenderingCreateInfo(..)
                                                             , RenderingInfo(..)
                                                             , RenderingAttachmentInfo(..)
                                                             , PhysicalDeviceDynamicRenderingFeatures(..)
                                                             , CommandBufferInheritanceRenderingInfo(..)
                                                             , AttachmentStoreOp(..)
                                                             , StructureType(..)
                                                             , RenderingFlagBits(..)
                                                             , RenderingFlags
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
import Vulkan.Core10.Enums.AttachmentLoadOp (AttachmentLoadOp)
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.CommandBufferBuilding (ClearValue)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginRendering))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndRendering))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Handles (ImageView)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled (MultisampledRenderToSingleSampledInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_dynamic_rendering (MultiviewPerViewAttributesInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas (MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_dynamic_rendering (RenderingFragmentDensityMapAttachmentInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_dynamic_rendering (RenderingFragmentShadingRateAttachmentInfoKHR)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_INFO))
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp(..))
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlagBits(..))
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRendering
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct RenderingInfo) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct RenderingInfo) -> IO ()

-- | vkCmdBeginRendering - Begin a dynamic render pass instance
--
-- = Description
--
-- After beginning a render pass instance, the command buffer is ready to
-- record
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#drawing draw commands>.
--
-- If @pRenderingInfo->flags@ includes
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_RESUMING_BIT' then this
-- render pass is resumed from a render pass instance that has been
-- suspended earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginRendering-dynamicRendering-06446# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicRendering dynamicRendering>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBeginRendering-commandBuffer-06068# If @commandBuffer@ is
--     a secondary command buffer, @pRenderingInfo->flags@ /must/ not
--     include
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginRendering-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginRendering-pRenderingInfo-parameter# @pRenderingInfo@
--     /must/ be a valid pointer to a valid 'RenderingInfo' structure
--
-- -   #VUID-vkCmdBeginRendering-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginRendering-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginRendering-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdBeginRendering-videocoding# This command /must/ only be
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'RenderingInfo'
cmdBeginRendering :: forall a io
                   . (Extendss RenderingInfo a, PokeChain a, MonadIO io)
                  => -- | @commandBuffer@ is the command buffer in which to record the command.
                     CommandBuffer
                  -> -- | @pRenderingInfo@ is a pointer to a 'RenderingInfo' structure specifying
                     -- details of the render pass instance to begin.
                     (RenderingInfo a)
                  -> io ()
cmdBeginRendering commandBuffer renderingInfo = liftIO . evalContT $ do
  let vkCmdBeginRenderingPtr = pVkCmdBeginRendering (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBeginRenderingPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginRendering is null" Nothing Nothing
  let vkCmdBeginRendering' = mkVkCmdBeginRendering vkCmdBeginRenderingPtr
  pRenderingInfo <- ContT $ withCStruct (renderingInfo)
  lift $ traceAroundEvent "vkCmdBeginRendering" (vkCmdBeginRendering'
                                                   (commandBufferHandle (commandBuffer))
                                                   (forgetExtensions pRenderingInfo))
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginRendering' and 'cmdEndRendering'
--
-- Note that 'cmdEndRendering' is *not* called if an exception is thrown by
-- the inner action.
cmdUseRendering :: forall a io r . (Extendss RenderingInfo a, PokeChain a, MonadIO io) => CommandBuffer -> RenderingInfo a -> io r -> io r
cmdUseRendering commandBuffer pRenderingInfo a =
  (cmdBeginRendering commandBuffer
                       pRenderingInfo) *> a <* (cmdEndRendering commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRendering
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndRendering - End a dynamic render pass instance
--
-- = Description
--
-- If the value of @pRenderingInfo->flags@ used to begin this render pass
-- instance included
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_SUSPENDING_BIT', then
-- this render pass is suspended and will be resumed later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndRendering-None-06161# The current render pass instance
--     /must/ have been begun with 'cmdBeginRendering'
--
-- -   #VUID-vkCmdEndRendering-commandBuffer-06162# The current render pass
--     instance /must/ have been begun in @commandBuffer@
--
-- -   #VUID-vkCmdEndRendering-None-06781# This command /must/ not be
--     recorded when transform feedback is active
--
-- -   #VUID-vkCmdEndRendering-None-06999# If
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery'* was called
--     within the render pass, the corresponding
--     'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery'* /must/ have been
--     called subsequently within the same subpass
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndRendering-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndRendering-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndRendering-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdEndRendering-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdEndRendering-videocoding# This command /must/ only be
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndRendering :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer in which to record the command.
                   CommandBuffer
                -> io ()
cmdEndRendering commandBuffer = liftIO $ do
  let vkCmdEndRenderingPtr = pVkCmdEndRendering (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdEndRenderingPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndRendering is null" Nothing Nothing
  let vkCmdEndRendering' = mkVkCmdEndRendering vkCmdEndRenderingPtr
  traceAroundEvent "vkCmdEndRendering" (vkCmdEndRendering'
                                          (commandBufferHandle (commandBuffer)))
  pure $ ()


-- | VkPipelineRenderingCreateInfo - Structure specifying attachment formats
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRenderingCreateInfo = PipelineRenderingCreateInfo
  { -- | @viewMask@ is the viewMask used for rendering.
    viewMask :: Word32
  , -- | @pColorAttachmentFormats@ is a pointer to an array of
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
deriving instance Generic (PipelineRenderingCreateInfo)
#endif
deriving instance Show PipelineRenderingCreateInfo

instance ToCStruct PipelineRenderingCreateInfo where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRenderingCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    f

instance FromCStruct PipelineRenderingCreateInfo where
  peekCStruct p = do
    viewMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pColorAttachmentFormats <- peek @(Ptr Format) ((p `plusPtr` 24 :: Ptr (Ptr Format)))
    pColorAttachmentFormats' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Format ((pColorAttachmentFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    depthAttachmentFormat <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    stencilAttachmentFormat <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    pure $ PipelineRenderingCreateInfo
             viewMask
             pColorAttachmentFormats'
             depthAttachmentFormat
             stencilAttachmentFormat

instance Zero PipelineRenderingCreateInfo where
  zero = PipelineRenderingCreateInfo
           zero
           mempty
           zero
           zero


-- | VkRenderingInfo - Structure specifying render pass instance begin info
--
-- = Description
--
-- If @viewMask@ is not @0@, multiview is enabled.
--
-- If there is an instance of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
-- included in the @pNext@ chain and its @deviceRenderAreaCount@ member is
-- not @0@, then @renderArea@ is ignored, and the render area is defined
-- per-device by that structure.
--
-- If multiview is enabled, and the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview-per-view-render-areas multiviewPerViewRenderAreas>
-- feature is enabled, and there is an instance of
-- 'Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas.MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM'
-- included in the @pNext@ chain with @perViewRenderAreaCount@ not equal to
-- @0@, then the elements of
-- 'Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas.MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM'::@pPerViewRenderAreas@
-- override @renderArea@ and define a render area for each view. In this
-- case, @renderArea@ /must/ be set to an area at least as large as the
-- union of all the per-view render areas.
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
-- -   #VUID-VkRenderingInfo-viewMask-06069# If @viewMask@ is @0@,
--     @layerCount@ /must/ not be @0@
--
-- -   #VUID-VkRenderingInfo-multisampledRenderToSingleSampled-06857# If
--     none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature are enabled, @imageView@ members of @pDepthAttachment@,
--     @pStencilAttachment@, and elements of @pColorAttachments@ that are
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been
--     created with the same @sampleCount@
--
-- -   #VUID-VkRenderingInfo-imageView-06858# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#subpass-multisampledrendertosinglesampled multisampled-render-to-single-sampled>
--     is enabled, then all attachments referenced by @imageView@ members
--     of @pDepthAttachment@, @pStencilAttachment@, and elements of
--     @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have a sample count
--     that is either
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' or
--     equal to
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'::@rasterizationSamples@
--
-- -   #VUID-VkRenderingInfo-imageView-06859# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#subpass-multisampledrendertosinglesampled multisampled-render-to-single-sampled>
--     is enabled, then all attachments referenced by @imageView@ members
--     of @pDepthAttachment@, @pStencilAttachment@, and elements of
--     @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT'
--     in their 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@
--
-- -   #VUID-VkRenderingInfo-pNext-06077# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.x@ /must/ be greater than or equal to 0
--
-- -   #VUID-VkRenderingInfo-pNext-06078# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.y@ /must/ be greater than or equal to 0
--
-- -   #VUID-VkRenderingInfo-pNext-07815# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0, the sum of
--     @renderArea.extent.width@ and @renderArea.offset.x@ /must/ be less
--     than or equal to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFramebufferWidth maxFramebufferWidth>
--
-- -   #VUID-VkRenderingInfo-pNext-07816# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0, the sum of
--     @renderArea.extent.height@ and @renderArea.offset.y@ /must/ be less
--     than or equal to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFramebufferWidth maxFramebufferHeight>
--
-- -   #VUID-VkRenderingInfo-pNext-06079# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0, the width of
--     the @imageView@ member of any element of @pColorAttachments@,
--     @pDepthAttachment@, or @pStencilAttachment@ that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be greater than or
--     equal to @renderArea.offset.x@ + @renderArea.extent.width@
--
-- -   #VUID-VkRenderingInfo-pNext-06080# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0, the height of
--     the @imageView@ member of any element of @pColorAttachments@,
--     @pDepthAttachment@, or @pStencilAttachment@ that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be greater than or
--     equal to @renderArea.offset.y@ + @renderArea.extent.height@
--
-- -   #VUID-VkRenderingInfo-pNext-06083# If the @pNext@ chain contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the width of the @imageView@ member of any element of
--     @pColorAttachments@, @pDepthAttachment@, or @pStencilAttachment@
--     that is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be
--     greater than or equal to the sum of the @offset.x@ and
--     @extent.width@ members of each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfo-pNext-06084# If the @pNext@ chain contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the height of the @imageView@ member of any element of
--     @pColorAttachments@, @pDepthAttachment@, or @pStencilAttachment@
--     that is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be
--     greater than or equal to the sum of the @offset.y@ and
--     @extent.height@ members of each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06085# If neither
--     @pDepthAttachment@ or @pStencilAttachment@ are @NULL@ and the
--     @imageView@ member of either structure is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @imageView@ member of
--     each structure /must/ be the same
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06086# If neither
--     @pDepthAttachment@ or @pStencilAttachment@ are @NULL@, and the
--     @resolveMode@ member of each is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', the
--     @resolveImageView@ member of each structure /must/ be the same
--
-- -   #VUID-VkRenderingInfo-colorAttachmentCount-06087# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', that @imageView@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06547# If @pDepthAttachment@
--     is not @NULL@ and @pDepthAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pDepthAttachment->imageView@ /must/ have been created with a format
--     that includes a depth component
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06088# If @pDepthAttachment@
--     is not @NULL@ and @pDepthAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pDepthAttachment->imageView@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-06548# If
--     @pStencilAttachment@ is not @NULL@ and
--     @pStencilAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pStencilAttachment->imageView@ /must/ have been created with a
--     format that includes a stencil aspect
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-06089# If
--     @pStencilAttachment@ is not @NULL@ and
--     @pStencilAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pStencilAttachment->imageView@ /must/ have been created with a
--     stencil usage including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkRenderingInfo-colorAttachmentCount-06090# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @layout@ member of
--     that element of @pColorAttachments@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-colorAttachmentCount-06091# If
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
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06092# If @pDepthAttachment@
--     is not @NULL@ and @pDepthAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @pDepthAttachment->layout@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06093# If @pDepthAttachment@
--     is not @NULL@, @pDepthAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pDepthAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pDepthAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-06094# If
--     @pStencilAttachment@ is not @NULL@ and
--     @pStencilAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pStencilAttachment->layout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-06095# If
--     @pStencilAttachment@ is not @NULL@, @pStencilAttachment->imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pStencilAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pStencilAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-colorAttachmentCount-06096# If
--     @colorAttachmentCount@ is not @0@ and the @imageView@ member of an
--     element of @pColorAttachments@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @layout@ member of
--     that element of @pColorAttachments@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-colorAttachmentCount-06097# If
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
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06098# If @pDepthAttachment@
--     is not @NULL@, @pDepthAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pDepthAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pDepthAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-06099# If
--     @pStencilAttachment@ is not @NULL@, @pStencilAttachment->imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pStencilAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pStencilAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-colorAttachmentCount-06100# If
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
-- -   #VUID-VkRenderingInfo-colorAttachmentCount-06101# If
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
-- -   #VUID-VkRenderingInfo-pDepthAttachment-07732# If @pDepthAttachment@
--     is not @NULL@ and @pDepthAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @pDepthAttachment->layout@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-07733# If @pDepthAttachment@
--     is not @NULL@, @pDepthAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pDepthAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pDepthAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-07734# If
--     @pStencilAttachment@ is not @NULL@ and
--     @pStencilAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pStencilAttachment->layout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-07735# If
--     @pStencilAttachment@ is not @NULL@, @pStencilAttachment->imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @pStencilAttachment->resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @pStencilAttachment->resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06102# If @pDepthAttachment@
--     is not @NULL@ and @pDepthAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pDepthAttachment->resolveMode@ /must/ be one of the bits set in
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties'::@supportedDepthResolveModes@
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-06103# If
--     @pStencilAttachment@ is not @NULL@ and
--     @pStencilAttachment->imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pStencilAttachment->resolveMode@ /must/ be one of the bits set in
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties'::@supportedStencilResolveModes@
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06104# If @pDepthAttachment@
--     or @pStencilAttachment@ are both not @NULL@,
--     @pDepthAttachment->imageView@ and @pStencilAttachment->imageView@
--     are both not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties'::@independentResolveNone@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', the @resolveMode@ of both
--     structures /must/ be the same value
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-06105# If @pDepthAttachment@
--     or @pStencilAttachment@ are both not @NULL@,
--     @pDepthAttachment->imageView@ and @pStencilAttachment->imageView@
--     are both not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties'::@independentResolve@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and the @resolveMode@ of
--     neither structure is
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', the
--     @resolveMode@ of both structures /must/ be the same value
--
-- -   #VUID-VkRenderingInfo-colorAttachmentCount-06106#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxColorAttachments@
--
-- -   #VUID-VkRenderingInfo-imageView-06107# If the @imageView@ member of
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fragmentDensityMapNonSubsampledImages fragmentDensityMapNonSubsampledImages>
--     feature is not enabled, valid @imageView@ and @resolveImageView@
--     members of @pDepthAttachment@, @pStencilAttachment@, and each
--     element of @pColorAttachments@ /must/ be a
--     'Vulkan.Core10.Handles.ImageView' created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkRenderingInfo-imageView-06108# If the @imageView@ member of
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and @viewMask@ is not @0@,
--     @imageView@ /must/ have a @layerCount@ greater than or equal to the
--     index of the most significant bit in @viewMask@
--
-- -   #VUID-VkRenderingInfo-imageView-06109# If the @imageView@ member of
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and @viewMask@ is @0@,
--     @imageView@ /must/ have a @layerCount@ equal to @1@
--
-- -   #VUID-VkRenderingInfo-pNext-06112# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0 and the
--     @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     width greater than or equal to
--     \(\left\lceil{\frac{renderArea_{x}+renderArea_{width}}{maxFragmentDensityTexelSize_{width}}}\right\rceil\)
--
-- -   #VUID-VkRenderingInfo-pNext-06113# If the @pNext@ chain contains a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     structure, its @deviceRenderAreaCount@ member is not 0, and the
--     @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     width greater than or equal to
--     \(\left\lceil{\frac{pDeviceRenderAreas_{x}+pDeviceRenderAreas_{width}}{maxFragmentDensityTexelSize_{width}}}\right\rceil\)
--     for each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfo-pNext-06114# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0 and the
--     @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     height greater than or equal to
--     \(\left\lceil{\frac{renderArea_{y}+renderArea_{height}}{maxFragmentDensityTexelSize_{height}}}\right\rceil\)
--
-- -   #VUID-VkRenderingInfo-pNext-06115# If the @pNext@ chain contains a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     structure, its @deviceRenderAreaCount@ member is not 0, and the
--     @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     height greater than or equal to
--     \(\left\lceil{\frac{pDeviceRenderAreas_{y}+pDeviceRenderAreas_{height}}{maxFragmentDensityTexelSize_{height}}}\right\rceil\)
--     for each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfo-imageView-06116# If the @imageView@ member of
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ not be equal to
--     the @imageView@ or @resolveImageView@ member of @pDepthAttachment@,
--     @pStencilAttachment@, or any element of @pColorAttachments@
--
-- -   #VUID-VkRenderingInfo-pNext-06119# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0 and the
--     @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     width greater than or equal to
--     \(\left\lceil{\frac{renderArea_{x}+renderArea_{width}}{shadingRateAttachmentTexelSize_{width}}}\right\rceil\)
--
-- -   #VUID-VkRenderingInfo-pNext-06120# If the @pNext@ chain contains a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     structure, its @deviceRenderAreaCount@ member is not 0, and the
--     @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     width greater than or equal to
--     \(\left\lceil{\frac{pDeviceRenderAreas_{x}+pDeviceRenderAreas_{width}}{shadingRateAttachmentTexelSize_{width}}}\right\rceil\)
--     for each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfo-pNext-06121# If the @pNext@ chain does not
--     contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0 and the
--     @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     height greater than or equal to
--     \(\left\lceil{\frac{renderArea_{y}+renderArea_{height}}{shadingRateAttachmentTexelSize_{height}}}\right\rceil\)
--
-- -   #VUID-VkRenderingInfo-pNext-06122# If the @pNext@ chain contains a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     structure, its @deviceRenderAreaCount@ member is not 0, and the
--     @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ have a
--     height greater than or equal to
--     \(\left\lceil{\frac{pDeviceRenderAreas_{y}+pDeviceRenderAreas_{height}}{shadingRateAttachmentTexelSize_{height}}}\right\rceil\)
--     for each element of @pDeviceRenderAreas@
--
-- -   #VUID-VkRenderingInfo-layerCount-07817# @layerCount@ /must/ be less
--     than or equal to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFramebufferLayers maxFramebufferLayers>
--
-- -   #VUID-VkRenderingInfo-imageView-06123# If the @imageView@ member of
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and @viewMask@ is @0@,
--     @imageView@ /must/ have a @layerCount@ that is either equal to @1@
--     or greater than or equal to @layerCount@
--
-- -   #VUID-VkRenderingInfo-imageView-06124# If the @imageView@ member of
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and @viewMask@ is not @0@,
--     @imageView@ /must/ have a @layerCount@ that either equal to @1@ or
--     greater than or equal to the index of the most significant bit in
--     @viewMask@
--
-- -   #VUID-VkRenderingInfo-imageView-06125# If the @imageView@ member of
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ not be equal to
--     the @imageView@ or @resolveImageView@ member of @pDepthAttachment@,
--     @pStencilAttachment@, or any element of @pColorAttachments@
--
-- -   #VUID-VkRenderingInfo-imageView-06126# If the @imageView@ member of
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--     structure included in the @pNext@ chain is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ not be equal to
--     the @imageView@ member of a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'
--     structure included in the @pNext@ chain
--
-- -   #VUID-VkRenderingInfo-multiview-06127# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview multiview>
--     feature is not enabled, @viewMask@ /must/ be @0@
--
-- -   #VUID-VkRenderingInfo-viewMask-06128# The index of the most
--     significant bit in @viewMask@ /must/ be less than
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxMultiviewViewCount maxMultiviewViewCount>
--
-- -   #VUID-VkRenderingInfo-perViewRenderAreaCount-07857# If the
--     @perViewRenderAreaCount@ member of a
--     'Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas.MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM'
--     structure included in the @pNext@ chain is not @0@, then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview-per-view-render-areas multiviewPerViewRenderAreas>
--     feature /must/ be enabled.
--
-- -   #VUID-VkRenderingInfo-perViewRenderAreaCount-07858# If the
--     @perViewRenderAreaCount@ member of a
--     'Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas.MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM'
--     structure included in the @pNext@ chain is not @0@, then
--     @renderArea@ /must/ specify a render area that includes the union of
--     all per view render areas.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_INFO'
--
-- -   #VUID-VkRenderingInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT',
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.MultiviewPerViewAttributesInfoNVX',
--     'Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas.MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM',
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT',
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'
--
-- -   #VUID-VkRenderingInfo-sType-unique# The @sType@ value of each struct
--     in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkRenderingInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits' values
--
-- -   #VUID-VkRenderingInfo-pColorAttachments-parameter# If
--     @colorAttachmentCount@ is not @0@, @pColorAttachments@ /must/ be a
--     valid pointer to an array of @colorAttachmentCount@ valid
--     'RenderingAttachmentInfo' structures
--
-- -   #VUID-VkRenderingInfo-pDepthAttachment-parameter# If
--     @pDepthAttachment@ is not @NULL@, @pDepthAttachment@ /must/ be a
--     valid pointer to a valid 'RenderingAttachmentInfo' structure
--
-- -   #VUID-VkRenderingInfo-pStencilAttachment-parameter# If
--     @pStencilAttachment@ is not @NULL@, @pStencilAttachment@ /must/ be a
--     valid pointer to a valid 'RenderingAttachmentInfo' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Rect2D', 'RenderingAttachmentInfo',
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdBeginRendering',
-- 'Vulkan.Extensions.VK_KHR_dynamic_rendering.cmdBeginRenderingKHR',
-- 'Vulkan.Extensions.VK_QCOM_tile_properties.getDynamicRenderingTilePropertiesQCOM'
data RenderingInfo (es :: [Type]) = RenderingInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits'.
    flags :: RenderingFlags
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
    -- 'RenderingAttachmentInfo' structures describing any color attachments
    -- used.
    colorAttachments :: Vector RenderingAttachmentInfo
  , -- | @pDepthAttachment@ is a pointer to a 'RenderingAttachmentInfo' structure
    -- describing a depth attachment.
    depthAttachment :: Maybe RenderingAttachmentInfo
  , -- | @pStencilAttachment@ is a pointer to a 'RenderingAttachmentInfo'
    -- structure describing a stencil attachment.
    stencilAttachment :: Maybe RenderingAttachmentInfo
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderingInfo es)

instance Extensible RenderingInfo where
  extensibleTypeName = "RenderingInfo"
  setNext RenderingInfo{..} next' = RenderingInfo{next = next', ..}
  getNext RenderingInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderingInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM = Just f
    | Just Refl <- eqT @e @MultiviewPerViewAttributesInfoNVX = Just f
    | Just Refl <- eqT @e @RenderingFragmentDensityMapAttachmentInfoEXT = Just f
    | Just Refl <- eqT @e @RenderingFragmentShadingRateAttachmentInfoKHR = Just f
    | Just Refl <- eqT @e @MultisampledRenderToSingleSampledInfoEXT = Just f
    | Just Refl <- eqT @e @DeviceGroupRenderPassBeginInfo = Just f
    | otherwise = Nothing

instance ( Extendss RenderingInfo es
         , PokeChain es ) => ToCStruct (RenderingInfo es) where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderingFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Rect2D)) (renderArea)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (layerCount)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachments)) :: Word32))
    pPColorAttachments' <- ContT $ allocaBytes @RenderingAttachmentInfo ((Data.Vector.length (colorAttachments)) * 72)
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPColorAttachments' `plusPtr` (72 * (i)) :: Ptr RenderingAttachmentInfo) (e) . ($ ())) (colorAttachments)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr RenderingAttachmentInfo))) (pPColorAttachments')
    pDepthAttachment'' <- case (depthAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr RenderingAttachmentInfo))) pDepthAttachment''
    pStencilAttachment'' <- case (stencilAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr RenderingAttachmentInfo))) pStencilAttachment''
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Rect2D)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ f

instance es ~ '[] => Zero (RenderingInfo es) where
  zero = RenderingInfo
           ()
           zero
           zero
           zero
           zero
           mempty
           Nothing
           Nothing


-- | VkRenderingAttachmentInfo - Structure specifying attachment information
--
-- = Description
--
-- Values in @imageView@ are loaded and stored according to the values of
-- @loadOp@ and @storeOp@, within the render area for each device specified
-- in 'RenderingInfo'. If @imageView@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', other members of this
-- structure are ignored; writes to this attachment will be discarded, and
-- no load, store, or resolve operations will be performed.
--
-- If @resolveMode@ is
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', then
-- @resolveImageView@ is ignored. If @resolveMode@ is not
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', and
-- @resolveImageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
-- values in @resolveImageView@ within the render area become undefined
-- once rendering begins. Only values in the aspect corresponding to the
-- use of this attachment become undefined (the depth aspect if this
-- attachment is used as 'RenderingInfo'::@pDepthAttachment@, and the
-- stencil aspect if it is used as @pStencilAttachment@).
--
-- At the end of rendering, the values written to each pixel location in
-- @imageView@ will be resolved according to @resolveMode@ and stored into
-- the same location in @resolveImageView@.
--
-- Note
--
-- The resolve mode and store operation are independent; it is valid to
-- write both resolved and unresolved values, and equally valid to discard
-- the unresolved values while writing the resolved ones.
--
-- Store and resolve operations are only performed at the end of a render
-- pass instance that does not specify the
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_SUSPENDING_BIT_KHR'
-- flag.
--
-- Load operations are only performed at the beginning of a render pass
-- instance that does not specify the
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_RESUMING_BIT_KHR' flag.
--
-- Image contents at the end of a suspended render pass instance remain
-- defined for access by a resuming render pass instance.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06129# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and has a non-integer
--     color format, @resolveMode@ /must/ be
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE' or
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_AVERAGE_BIT'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06130# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and has an integer
--     color format, @resolveMode@ /must/ be
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE' or
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06861# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', and the
--     @pNext@ chain of 'RenderingInfo' does not includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with the @multisampledRenderToSingleSampledEnable@ field
--     equal to 'Vulkan.Core10.FundamentalTypes.TRUE', @imageView@ /must/
--     not have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06862# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', and the
--     @pNext@ chain of 'RenderingInfo' does not includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with the @multisampledRenderToSingleSampledEnable@ field
--     equal to 'Vulkan.Core10.FundamentalTypes.TRUE', @resolveImageView@
--     /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06863# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @resolveMode@ is not
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', the
--     @pNext@ chain of 'RenderingInfo' includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with the @multisampledRenderToSingleSampledEnable@ field
--     equal to 'Vulkan.Core10.FundamentalTypes.TRUE', and @imageView@ has
--     a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT',
--     @resolveImageView@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06864# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @resolveImageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageView@ /must/ have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06865# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @resolveImageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @imageView@ and @resolveImageView@ /must/ have the same
--     'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06135# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@ /must/
--     not be 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06136# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06137# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06138# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@ /must/
--     not be
--     'Vulkan.Extensions.VK_NV_shading_rate_image.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06139# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Extensions.VK_NV_shading_rate_image.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06140# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@ /must/
--     not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06141# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06142# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06143# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@ /must/
--     not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06144# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06145# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@ /must/
--     not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR'
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-06146# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and @resolveMode@ is
--     not 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE',
--     @resolveImageLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingAttachmentInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO'
--
-- -   #VUID-VkRenderingAttachmentInfo-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkRenderingAttachmentInfo-imageView-parameter# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/
--     be a valid 'Vulkan.Core10.Handles.ImageView' handle
--
-- -   #VUID-VkRenderingAttachmentInfo-imageLayout-parameter# @imageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkRenderingAttachmentInfo-resolveMode-parameter# If
--     @resolveMode@ is not @0@, @resolveMode@ /must/ be a valid
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
--
-- -   #VUID-VkRenderingAttachmentInfo-resolveImageView-parameter# If
--     @resolveImageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @resolveImageView@ /must/ be a valid
--     'Vulkan.Core10.Handles.ImageView' handle
--
-- -   #VUID-VkRenderingAttachmentInfo-resolveImageLayout-parameter#
--     @resolveImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkRenderingAttachmentInfo-loadOp-parameter# @loadOp@ /must/ be
--     a valid 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp'
--     value
--
-- -   #VUID-VkRenderingAttachmentInfo-storeOp-parameter# @storeOp@ /must/
--     be a valid 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
--     value
--
-- -   #VUID-VkRenderingAttachmentInfo-clearValue-parameter# @clearValue@
--     /must/ be a valid 'Vulkan.Core10.CommandBufferBuilding.ClearValue'
--     union
--
-- -   #VUID-VkRenderingAttachmentInfo-commonparent# Both of @imageView@,
--     and @resolveImageView@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp',
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp',
-- 'Vulkan.Core10.CommandBufferBuilding.ClearValue',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView', 'RenderingInfo',
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderingAttachmentInfo = RenderingAttachmentInfo
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
    -- structure defining values used to clear @imageView@ when @loadOp@ is
    -- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'.
    clearValue :: ClearValue
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingAttachmentInfo)
#endif
deriving instance Show RenderingAttachmentInfo

instance ToCStruct RenderingAttachmentInfo where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingAttachmentInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr AttachmentLoadOp)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr AttachmentStoreOp)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 52 :: Ptr ClearValue)) (zero) . ($ ())
    lift $ f

instance Zero RenderingAttachmentInfo where
  zero = RenderingAttachmentInfo
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceDynamicRenderingFeatures - Structure indicating support
-- for dynamic render pass instances
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDynamicRenderingFeatures' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDynamicRenderingFeatures' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDynamicRenderingFeatures = PhysicalDeviceDynamicRenderingFeatures
  { -- | #extension-features-dynamicRendering# @dynamicRendering@ specifies that
    -- the implementation supports dynamic render pass instances using the
    -- 'cmdBeginRendering' command.
    dynamicRendering :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDynamicRenderingFeatures)
#endif
deriving instance Show PhysicalDeviceDynamicRenderingFeatures

instance ToCStruct PhysicalDeviceDynamicRenderingFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDynamicRenderingFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dynamicRendering))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDynamicRenderingFeatures where
  peekCStruct p = do
    dynamicRendering <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDynamicRenderingFeatures
             (bool32ToBool dynamicRendering)

instance Storable PhysicalDeviceDynamicRenderingFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDynamicRenderingFeatures where
  zero = PhysicalDeviceDynamicRenderingFeatures
           zero


-- | VkCommandBufferInheritanceRenderingInfo - Structure specifying command
-- buffer inheritance info for dynamic render pass instances
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes a
-- 'CommandBufferInheritanceRenderingInfo' structure, then that structure
-- controls parameters of dynamic render pass instances that the
-- 'Vulkan.Core10.Handles.CommandBuffer' /can/ be executed within. If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@renderPass@
-- is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', or
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is not specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@flags@,
-- parameters of this structure are ignored.
--
-- If @colorAttachmentCount@ is @0@ and the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-variableMultisampleRate variableMultisampleRate>
-- feature is enabled, @rasterizationSamples@ is ignored.
--
-- If @depthAttachmentFormat@, @stencilAttachmentFormat@, or any element of
-- @pColorAttachmentFormats@ is
-- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it indicates that the
-- corresponding attachment is unused within the render pass.
--
-- == Valid Usage
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-colorAttachmentCount-06004#
--     If @colorAttachmentCount@ is not @0@, @rasterizationSamples@ /must/
--     be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-variableMultisampleRate-06005#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-variableMultisampleRate variableMultisampleRate>
--     feature is not enabled, @rasterizationSamples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-depthAttachmentFormat-06540#
--     If @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     that includes a depth component
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-depthAttachmentFormat-06007#
--     If @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfoKHR-pColorAttachmentFormats-06492#
--     If any element of @pColorAttachmentFormats@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT',
--     or
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
--     if the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-linearColorAttachment linearColorAttachment>
--     feature is enabled
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-stencilAttachmentFormat-06541#
--     If @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     that includes a stencil aspect
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-stencilAttachmentFormat-06199#
--     If @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-depthAttachmentFormat-06200#
--     If @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' and
--     @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     @depthAttachmentFormat@ /must/ equal @stencilAttachmentFormat@
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-multiview-06008# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview multiview>
--     feature is not enabled, @viewMask@ /must/ be @0@
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-viewMask-06009# The
--     index of the most significant bit in @viewMask@ /must/ be less than
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxMultiviewViewCount maxMultiviewViewCount>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO'
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-flags-parameter#
--     @flags@ /must/ be a valid combination of
--     'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits' values
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-pColorAttachmentFormats-parameter#
--     If @colorAttachmentCount@ is not @0@, @pColorAttachmentFormats@
--     /must/ be a valid pointer to an array of @colorAttachmentCount@
--     valid 'Vulkan.Core10.Enums.Format.Format' values
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-depthAttachmentFormat-parameter#
--     @depthAttachmentFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-stencilAttachmentFormat-parameter#
--     @stencilAttachmentFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkCommandBufferInheritanceRenderingInfo-rasterizationSamples-parameter#
--     If @rasterizationSamples@ is not @0@, @rasterizationSamples@ /must/
--     be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlags',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data CommandBufferInheritanceRenderingInfo = CommandBufferInheritanceRenderingInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits' used by the
    -- render pass instance.
    flags :: RenderingFlags
  , -- | @viewMask@ is the view mask used for rendering.
    viewMask :: Word32
  , -- | @pColorAttachmentFormats@ is a pointer to an array of
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
deriving instance Generic (CommandBufferInheritanceRenderingInfo)
#endif
deriving instance Show CommandBufferInheritanceRenderingInfo

instance ToCStruct CommandBufferInheritanceRenderingInfo where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceRenderingInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderingFlags)) (flags)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Format)) (zero)
    f

instance FromCStruct CommandBufferInheritanceRenderingInfo where
  peekCStruct p = do
    flags <- peek @RenderingFlags ((p `plusPtr` 16 :: Ptr RenderingFlags))
    viewMask <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pColorAttachmentFormats <- peek @(Ptr Format) ((p `plusPtr` 32 :: Ptr (Ptr Format)))
    pColorAttachmentFormats' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Format ((pColorAttachmentFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    depthAttachmentFormat <- peek @Format ((p `plusPtr` 40 :: Ptr Format))
    stencilAttachmentFormat <- peek @Format ((p `plusPtr` 44 :: Ptr Format))
    rasterizationSamples <- peek @SampleCountFlagBits ((p `plusPtr` 48 :: Ptr SampleCountFlagBits))
    pure $ CommandBufferInheritanceRenderingInfo
             flags
             viewMask
             pColorAttachmentFormats'
             depthAttachmentFormat
             stencilAttachmentFormat
             rasterizationSamples

instance Zero CommandBufferInheritanceRenderingInfo where
  zero = CommandBufferInheritanceRenderingInfo
           zero
           zero
           mempty
           zero
           zero
           zero

