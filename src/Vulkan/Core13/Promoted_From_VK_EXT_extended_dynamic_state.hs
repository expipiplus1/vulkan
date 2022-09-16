{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_extended_dynamic_state"
module Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state  ( cmdSetCullMode
                                                                  , cmdSetFrontFace
                                                                  , cmdSetPrimitiveTopology
                                                                  , cmdSetViewportWithCount
                                                                  , cmdSetScissorWithCount
                                                                  , cmdBindVertexBuffers2
                                                                  , cmdSetDepthTestEnable
                                                                  , cmdSetDepthWriteEnable
                                                                  , cmdSetDepthCompareOp
                                                                  , cmdSetDepthBoundsTestEnable
                                                                  , cmdSetStencilTestEnable
                                                                  , cmdSetStencilOp
                                                                  , DynamicState(..)
                                                                  ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
import Foreign.Storable (Storable(poke))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.CompareOp (CompareOp)
import Vulkan.Core10.Enums.CompareOp (CompareOp(..))
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlagBits(..))
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlags)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindVertexBuffers2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCullMode))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBoundsTestEnable))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthCompareOp))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthTestEnable))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthWriteEnable))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetFrontFace))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPrimitiveTopology))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetScissorWithCount))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilOp))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilTestEnable))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportWithCount))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.FrontFace (FrontFace)
import Vulkan.Core10.Enums.FrontFace (FrontFace(..))
import Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology)
import Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlagBits(..))
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import Vulkan.Core10.Enums.StencilOp (StencilOp)
import Vulkan.Core10.Enums.StencilOp (StencilOp(..))
import Vulkan.Core10.Pipeline (Viewport)
import Vulkan.Core10.Enums.DynamicState (DynamicState(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCullMode
  :: FunPtr (Ptr CommandBuffer_T -> CullModeFlags -> IO ()) -> Ptr CommandBuffer_T -> CullModeFlags -> IO ()

-- | vkCmdSetCullMode - Set cull mode dynamically for a command buffer
--
-- = Description
--
-- This command sets the cull mode for subsequent drawing commands when the
-- graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@cullMode@
-- value used to create the currently active pipeline.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCullMode-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCullMode-cullMode-parameter# @cullMode@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlagBits' values
--
-- -   #VUID-vkCmdSetCullMode-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCullMode-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetCullMode-videocoding# This command /must/ only be
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlags'
cmdSetCullMode :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer into which the command will be
                  -- recorded.
                  CommandBuffer
               -> -- | @cullMode@ specifies the cull mode property to use for drawing.
                  CullModeFlags
               -> io ()
cmdSetCullMode commandBuffer cullMode = liftIO $ do
  let vkCmdSetCullModePtr = pVkCmdSetCullMode (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetCullModePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCullMode is null" Nothing Nothing
  let vkCmdSetCullMode' = mkVkCmdSetCullMode vkCmdSetCullModePtr
  traceAroundEvent "vkCmdSetCullMode" (vkCmdSetCullMode' (commandBufferHandle (commandBuffer)) (cullMode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetFrontFace
  :: FunPtr (Ptr CommandBuffer_T -> FrontFace -> IO ()) -> Ptr CommandBuffer_T -> FrontFace -> IO ()

-- | vkCmdSetFrontFace - Set front face orientation dynamically for a command
-- buffer
--
-- = Description
--
-- This command sets the front face orientation for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@frontFace@
-- value used to create the currently active pipeline.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetFrontFace-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetFrontFace-frontFace-parameter# @frontFace@ /must/ be a
--     valid 'Vulkan.Core10.Enums.FrontFace.FrontFace' value
--
-- -   #VUID-vkCmdSetFrontFace-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetFrontFace-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetFrontFace-videocoding# This command /must/ only be
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.FrontFace.FrontFace'
cmdSetFrontFace :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command will be
                   -- recorded.
                   CommandBuffer
                -> -- | @frontFace@ is a 'Vulkan.Core10.Enums.FrontFace.FrontFace' value
                   -- specifying the front-facing triangle orientation to be used for culling.
                   FrontFace
                -> io ()
cmdSetFrontFace commandBuffer frontFace = liftIO $ do
  let vkCmdSetFrontFacePtr = pVkCmdSetFrontFace (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetFrontFacePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetFrontFace is null" Nothing Nothing
  let vkCmdSetFrontFace' = mkVkCmdSetFrontFace vkCmdSetFrontFacePtr
  traceAroundEvent "vkCmdSetFrontFace" (vkCmdSetFrontFace' (commandBufferHandle (commandBuffer)) (frontFace))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPrimitiveTopology
  :: FunPtr (Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()) -> Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()

-- | vkCmdSetPrimitiveTopology - Set primitive topology state dynamically for
-- a command buffer
--
-- = Description
--
-- This command sets the primitive topology for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
-- value used to create the currently active pipeline.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetPrimitiveTopology-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetPrimitiveTopology-primitiveTopology-parameter#
--     @primitiveTopology@ /must/ be a valid
--     'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology' value
--
-- -   #VUID-vkCmdSetPrimitiveTopology-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetPrimitiveTopology-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetPrimitiveTopology-videocoding# This command /must/
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology'
cmdSetPrimitiveTopology :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @primitiveTopology@ specifies the primitive topology to use for drawing.
                           PrimitiveTopology
                        -> io ()
cmdSetPrimitiveTopology commandBuffer primitiveTopology = liftIO $ do
  let vkCmdSetPrimitiveTopologyPtr = pVkCmdSetPrimitiveTopology (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetPrimitiveTopologyPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPrimitiveTopology is null" Nothing Nothing
  let vkCmdSetPrimitiveTopology' = mkVkCmdSetPrimitiveTopology vkCmdSetPrimitiveTopologyPtr
  traceAroundEvent "vkCmdSetPrimitiveTopology" (vkCmdSetPrimitiveTopology' (commandBufferHandle (commandBuffer)) (primitiveTopology))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportWithCount
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Viewport -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Viewport -> IO ()

-- | vkCmdSetViewportWithCount - Set the viewport count and viewports
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the viewport count and viewports state for subsequent
-- drawing commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the corresponding
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
-- and @pViewports@ values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetViewportWithCount-viewportCount-03394# @viewportCount@
--     /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetViewportWithCount-viewportCount-03395# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiViewport multiViewport>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- -   #VUID-vkCmdSetViewportWithCount-commandBuffer-04819# @commandBuffer@
--     /must/ not have
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV'::@viewportScissor2D@
--     enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetViewportWithCount-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetViewportWithCount-pViewports-parameter# @pViewports@
--     /must/ be a valid pointer to an array of @viewportCount@ valid
--     'Vulkan.Core10.Pipeline.Viewport' structures
--
-- -   #VUID-vkCmdSetViewportWithCount-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetViewportWithCount-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetViewportWithCount-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetViewportWithCount-viewportCount-arraylength#
--     @viewportCount@ /must/ be greater than @0@
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Pipeline.Viewport'
cmdSetViewportWithCount :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @pViewports@ specifies the viewports to use for drawing.
                           ("viewports" ::: Vector Viewport)
                        -> io ()
cmdSetViewportWithCount commandBuffer viewports = liftIO . evalContT $ do
  let vkCmdSetViewportWithCountPtr = pVkCmdSetViewportWithCount (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetViewportWithCountPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportWithCount is null" Nothing Nothing
  let vkCmdSetViewportWithCount' = mkVkCmdSetViewportWithCount vkCmdSetViewportWithCountPtr
  pPViewports <- ContT $ allocaBytes @Viewport ((Data.Vector.length (viewports)) * 24)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e)) (viewports)
  lift $ traceAroundEvent "vkCmdSetViewportWithCount" (vkCmdSetViewportWithCount' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (viewports)) :: Word32)) (pPViewports))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissorWithCount
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetScissorWithCount - Set the scissor count and scissor rectangular
-- bounds dynamically for a command buffer
--
-- = Description
--
-- This command sets the scissor count and scissor rectangular bounds state
-- for subsequence drawing commands when the graphics pipeline is created
-- with 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the corresponding
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
-- and @pScissors@ values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetScissorWithCount-scissorCount-03397# @scissorCount@
--     /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetScissorWithCount-scissorCount-03398# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiViewport multiViewport>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   #VUID-vkCmdSetScissorWithCount-x-03399# The @x@ and @y@ members of
--     @offset@ member of any element of @pScissors@ /must/ be greater than
--     or equal to @0@
--
-- -   #VUID-vkCmdSetScissorWithCount-offset-03400# Evaluation of
--     (@offset.x@ + @extent.width@) /must/ not cause a signed integer
--     addition overflow for any element of @pScissors@
--
-- -   #VUID-vkCmdSetScissorWithCount-offset-03401# Evaluation of
--     (@offset.y@ + @extent.height@) /must/ not cause a signed integer
--     addition overflow for any element of @pScissors@
--
-- -   #VUID-vkCmdSetScissorWithCount-commandBuffer-04820# @commandBuffer@
--     /must/ not have
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV'::@viewportScissor2D@
--     enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetScissorWithCount-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetScissorWithCount-pScissors-parameter# @pScissors@
--     /must/ be a valid pointer to an array of @scissorCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   #VUID-vkCmdSetScissorWithCount-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetScissorWithCount-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetScissorWithCount-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetScissorWithCount-scissorCount-arraylength#
--     @scissorCount@ /must/ be greater than @0@
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D'
cmdSetScissorWithCount :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command will be
                          -- recorded.
                          CommandBuffer
                       -> -- | @pScissors@ specifies the scissors to use for drawing.
                          ("scissors" ::: Vector Rect2D)
                       -> io ()
cmdSetScissorWithCount commandBuffer scissors = liftIO . evalContT $ do
  let vkCmdSetScissorWithCountPtr = pVkCmdSetScissorWithCount (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetScissorWithCountPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetScissorWithCount is null" Nothing Nothing
  let vkCmdSetScissorWithCount' = mkVkCmdSetScissorWithCount vkCmdSetScissorWithCountPtr
  pPScissors <- ContT $ allocaBytes @Rect2D ((Data.Vector.length (scissors)) * 16)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (scissors)
  lift $ traceAroundEvent "vkCmdSetScissorWithCount" (vkCmdSetScissorWithCount' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (scissors)) :: Word32)) (pPScissors))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindVertexBuffers2
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()

-- | vkCmdBindVertexBuffers2 - Bind vertex buffers to a command buffer and
-- dynamically set strides
--
-- = Description
--
-- The values taken from elements i of @pBuffers@ and @pOffsets@ replace
-- the current state for the vertex input binding @firstBinding@ + i, for i
-- in [0, @bindingCount@). The vertex input binding is updated to start at
-- the offset indicated by @pOffsets@[i] from the start of the buffer
-- @pBuffers@[i]. If @pSizes@ is not @NULL@ then @pSizes@[i] specifies the
-- bound size of the vertex buffer starting from the corresponding elements
-- of @pBuffers@[i] plus @pOffsets@[i]. All vertex input attributes that
-- use each of these bindings will use these updated addresses in their
-- address calculations for subsequent drawing commands. If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
-- feature is enabled, elements of @pBuffers@ /can/ be
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', and /can/ be used by the
-- vertex shader. If a vertex input attribute is bound to a vertex input
-- binding that is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the values
-- taken from memory are considered to be zero, and missing G, B, or A
-- components are
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input-extraction filled with (0,0,1)>.
--
-- This command also
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-dynamic-state dynamically sets>
-- the byte strides between consecutive elements within buffer
-- @pBuffers@[i] to the corresponding @pStrides@[i] value when the graphics
-- pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, strides are specified by the
-- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription'::@stride@ values
-- used to create the currently active pipeline.
--
-- If the bound pipeline state object was also created with the
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
-- dynamic state enabled then
-- 'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
-- /can/ be used instead of 'cmdBindVertexBuffers2' to set the stride.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindVertexBuffers2-firstBinding-03355# @firstBinding@
--     /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-vkCmdBindVertexBuffers2-firstBinding-03356# The sum of
--     @firstBinding@ and @bindingCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-vkCmdBindVertexBuffers2-pOffsets-03357# All elements of
--     @pOffsets@ /must/ be less than the size of the corresponding element
--     in @pBuffers@
--
-- -   #VUID-vkCmdBindVertexBuffers2-pSizes-03358# If @pSizes@ is not
--     @NULL@, all elements of @pOffsets@ plus @pSizes@ /must/ be less than
--     or equal to the size of the corresponding element in @pBuffers@
--
-- -   #VUID-vkCmdBindVertexBuffers2-pBuffers-03359# All elements of
--     @pBuffers@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdBindVertexBuffers2-pBuffers-03360# Each element of
--     @pBuffers@ that is non-sparse /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBindVertexBuffers2-pBuffers-04111# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all elements of @pBuffers@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindVertexBuffers2-pBuffers-04112# If an element of
--     @pBuffers@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     corresponding element of @pOffsets@ /must/ be zero
--
-- -   #VUID-vkCmdBindVertexBuffers2-pStrides-03362# If @pStrides@ is not
--     @NULL@ each element of @pStrides@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindingStride@
--
-- -   #VUID-vkCmdBindVertexBuffers2-pStrides-06209# If @pStrides@ is not
--     @NULL@ each element of @pStrides@ /must/ be either 0 or greater than
--     or equal to the maximum extent of all vertex input attributes
--     fetched from the corresponding binding, where the extent is
--     calculated as the
--     'Vulkan.Core10.Pipeline.VertexInputAttributeDescription'::@offset@
--     plus
--     'Vulkan.Core10.Pipeline.VertexInputAttributeDescription'::@format@
--     size
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindVertexBuffers2-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindVertexBuffers2-pBuffers-parameter# @pBuffers@ /must/
--     be a valid pointer to an array of @bindingCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Core10.Handles.Buffer' handles
--
-- -   #VUID-vkCmdBindVertexBuffers2-pOffsets-parameter# @pOffsets@ /must/
--     be a valid pointer to an array of @bindingCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBindVertexBuffers2-pSizes-parameter# If @pSizes@ is not
--     @NULL@, @pSizes@ /must/ be a valid pointer to an array of
--     @bindingCount@ 'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBindVertexBuffers2-pStrides-parameter# If @pStrides@ is
--     not @NULL@, @pStrides@ /must/ be a valid pointer to an array of
--     @bindingCount@ 'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBindVertexBuffers2-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindVertexBuffers2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindVertexBuffers2-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdBindVertexBuffers2-bindingCount-arraylength# If any of
--     @pSizes@, or @pStrides@ are not @NULL@, @bindingCount@ /must/ be
--     greater than @0@
--
-- -   #VUID-vkCmdBindVertexBuffers2-commonparent# Both of @commandBuffer@,
--     and the elements of @pBuffers@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdBindVertexBuffers2 :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command is
                         -- recorded.
                         CommandBuffer
                      -> -- | @firstBinding@ is the index of the first vertex input binding whose
                         -- state is updated by the command.
                         ("firstBinding" ::: Word32)
                      -> -- | @pBuffers@ is a pointer to an array of buffer handles.
                         ("buffers" ::: Vector Buffer)
                      -> -- | @pOffsets@ is a pointer to an array of buffer offsets.
                         ("offsets" ::: Vector DeviceSize)
                      -> -- | @pSizes@ is @NULL@ or a pointer to an array of the size in bytes of
                         -- vertex data bound from @pBuffers@.
                         ("sizes" ::: Vector DeviceSize)
                      -> -- | @pStrides@ is @NULL@ or a pointer to an array of buffer strides.
                         ("strides" ::: Vector DeviceSize)
                      -> io ()
cmdBindVertexBuffers2 commandBuffer firstBinding buffers offsets sizes strides = liftIO . evalContT $ do
  let vkCmdBindVertexBuffers2Ptr = pVkCmdBindVertexBuffers2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindVertexBuffers2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindVertexBuffers2 is null" Nothing Nothing
  let vkCmdBindVertexBuffers2' = mkVkCmdBindVertexBuffers2 vkCmdBindVertexBuffers2Ptr
  let pBuffersLength = Data.Vector.length $ (buffers)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  let pSizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral pSizesLength == pBuffersLength || pSizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pSizes and pBuffers must have the same length" Nothing Nothing
  let pStridesLength = Data.Vector.length $ (strides)
  lift $ unless (fromIntegral pStridesLength == pBuffersLength || pStridesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pStrides and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytes @Buffer ((Data.Vector.length (buffers)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytes @DeviceSize ((Data.Vector.length (offsets)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  pSizes <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pPSizes <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (sizes))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pPSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pPSizes
  pStrides <- if Data.Vector.null (strides)
    then pure nullPtr
    else do
      pPStrides <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (strides))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pPStrides `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((strides))
      pure $ pPStrides
  lift $ traceAroundEvent "vkCmdBindVertexBuffers2" (vkCmdBindVertexBuffers2' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets) pSizes pStrides)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthTestEnable
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthTestEnable - Set depth test enable dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the depth test enable for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthTestEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthTestEnable-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthTestEnable-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthTestEnable-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthTestEnable-videocoding# This command /must/ only
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthTestEnable :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @depthTestEnable@ specifies if the depth test is enabled.
                         ("depthTestEnable" ::: Bool)
                      -> io ()
cmdSetDepthTestEnable commandBuffer depthTestEnable = liftIO $ do
  let vkCmdSetDepthTestEnablePtr = pVkCmdSetDepthTestEnable (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDepthTestEnablePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthTestEnable is null" Nothing Nothing
  let vkCmdSetDepthTestEnable' = mkVkCmdSetDepthTestEnable vkCmdSetDepthTestEnablePtr
  traceAroundEvent "vkCmdSetDepthTestEnable" (vkCmdSetDepthTestEnable' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthTestEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthWriteEnable
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthWriteEnable - Set depth write enable dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the depth write enable for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthWriteEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthWriteEnable-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthWriteEnable-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthWriteEnable-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthWriteEnable-videocoding# This command /must/ only
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthWriteEnable :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command will be
                          -- recorded.
                          CommandBuffer
                       -> -- | @depthWriteEnable@ specifies if depth writes are enabled.
                          ("depthWriteEnable" ::: Bool)
                       -> io ()
cmdSetDepthWriteEnable commandBuffer depthWriteEnable = liftIO $ do
  let vkCmdSetDepthWriteEnablePtr = pVkCmdSetDepthWriteEnable (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDepthWriteEnablePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthWriteEnable is null" Nothing Nothing
  let vkCmdSetDepthWriteEnable' = mkVkCmdSetDepthWriteEnable vkCmdSetDepthWriteEnablePtr
  traceAroundEvent "vkCmdSetDepthWriteEnable" (vkCmdSetDepthWriteEnable' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthWriteEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthCompareOp
  :: FunPtr (Ptr CommandBuffer_T -> CompareOp -> IO ()) -> Ptr CommandBuffer_T -> CompareOp -> IO ()

-- | vkCmdSetDepthCompareOp - Set depth comparison operator dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the depth comparison operator for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthCompareOp@
-- value used to create the currently active pipeline.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthCompareOp-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthCompareOp-depthCompareOp-parameter#
--     @depthCompareOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   #VUID-vkCmdSetDepthCompareOp-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthCompareOp-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthCompareOp-videocoding# This command /must/ only
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp'
cmdSetDepthCompareOp :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @depthCompareOp@ is a 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
                        -- specifying the comparison operator used for the
                        -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-depth-comparison Depth Comparison>
                        -- step of the
                        -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-depth depth test>.
                        ("depthCompareOp" ::: CompareOp)
                     -> io ()
cmdSetDepthCompareOp commandBuffer depthCompareOp = liftIO $ do
  let vkCmdSetDepthCompareOpPtr = pVkCmdSetDepthCompareOp (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDepthCompareOpPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthCompareOp is null" Nothing Nothing
  let vkCmdSetDepthCompareOp' = mkVkCmdSetDepthCompareOp vkCmdSetDepthCompareOpPtr
  traceAroundEvent "vkCmdSetDepthCompareOp" (vkCmdSetDepthCompareOp' (commandBufferHandle (commandBuffer)) (depthCompareOp))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBoundsTestEnable
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthBoundsTestEnable - Set depth bounds test enable dynamically
-- for a command buffer
--
-- = Description
--
-- This command sets the depth bounds enable for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthBoundsTestEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthBoundsTestEnable-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthBoundsTestEnable-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthBoundsTestEnable-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthBoundsTestEnable-videocoding# This command /must/
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthBoundsTestEnable :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command will be
                               -- recorded.
                               CommandBuffer
                            -> -- | @depthBoundsTestEnable@ specifies if the depth bounds test is enabled.
                               ("depthBoundsTestEnable" ::: Bool)
                            -> io ()
cmdSetDepthBoundsTestEnable commandBuffer depthBoundsTestEnable = liftIO $ do
  let vkCmdSetDepthBoundsTestEnablePtr = pVkCmdSetDepthBoundsTestEnable (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDepthBoundsTestEnablePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBoundsTestEnable is null" Nothing Nothing
  let vkCmdSetDepthBoundsTestEnable' = mkVkCmdSetDepthBoundsTestEnable vkCmdSetDepthBoundsTestEnablePtr
  traceAroundEvent "vkCmdSetDepthBoundsTestEnable" (vkCmdSetDepthBoundsTestEnable' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthBoundsTestEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilTestEnable
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetStencilTestEnable - Set stencil test enable dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the stencil test enable for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@stencilTestEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetStencilTestEnable-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetStencilTestEnable-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetStencilTestEnable-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetStencilTestEnable-videocoding# This command /must/
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetStencilTestEnable :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @stencilTestEnable@ specifies if the stencil test is enabled.
                           ("stencilTestEnable" ::: Bool)
                        -> io ()
cmdSetStencilTestEnable commandBuffer stencilTestEnable = liftIO $ do
  let vkCmdSetStencilTestEnablePtr = pVkCmdSetStencilTestEnable (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetStencilTestEnablePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilTestEnable is null" Nothing Nothing
  let vkCmdSetStencilTestEnable' = mkVkCmdSetStencilTestEnable vkCmdSetStencilTestEnablePtr
  traceAroundEvent "vkCmdSetStencilTestEnable" (vkCmdSetStencilTestEnable' (commandBufferHandle (commandBuffer)) (boolToBool32 (stencilTestEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilOp
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> StencilOp -> StencilOp -> StencilOp -> CompareOp -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> StencilOp -> StencilOp -> StencilOp -> CompareOp -> IO ()

-- | vkCmdSetStencilOp - Set stencil operation dynamically for a command
-- buffer
--
-- = Description
--
-- This command sets the stencil operation for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the corresponding
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@failOp@,
-- @passOp@, @depthFailOp@, and @compareOp@ values used to create the
-- currently active pipeline, for both front and back faces.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetStencilOp-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetStencilOp-faceMask-parameter# @faceMask@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' values
--
-- -   #VUID-vkCmdSetStencilOp-faceMask-requiredbitmask# @faceMask@ /must/
--     not be @0@
--
-- -   #VUID-vkCmdSetStencilOp-failOp-parameter# @failOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.StencilOp.StencilOp' value
--
-- -   #VUID-vkCmdSetStencilOp-passOp-parameter# @passOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.StencilOp.StencilOp' value
--
-- -   #VUID-vkCmdSetStencilOp-depthFailOp-parameter# @depthFailOp@ /must/
--     be a valid 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
--
-- -   #VUID-vkCmdSetStencilOp-compareOp-parameter# @compareOp@ /must/ be a
--     valid 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   #VUID-vkCmdSetStencilOp-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetStencilOp-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetStencilOp-videocoding# This command /must/ only be
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags',
-- 'Vulkan.Core10.Enums.StencilOp.StencilOp'
cmdSetStencilOp :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command will be
                   -- recorded.
                   CommandBuffer
                -> -- | @faceMask@ is a bitmask of
                   -- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' specifying
                   -- the set of stencil state for which to update the stencil operation.
                   ("faceMask" ::: StencilFaceFlags)
                -> -- | @failOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value specifying
                   -- the action performed on samples that fail the stencil test.
                   ("failOp" ::: StencilOp)
                -> -- | @passOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value specifying
                   -- the action performed on samples that pass both the depth and stencil
                   -- tests.
                   ("passOp" ::: StencilOp)
                -> -- | @depthFailOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
                   -- specifying the action performed on samples that pass the stencil test
                   -- and fail the depth test.
                   ("depthFailOp" ::: StencilOp)
                -> -- | @compareOp@ is a 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
                   -- specifying the comparison operator used in the stencil test.
                   CompareOp
                -> io ()
cmdSetStencilOp commandBuffer faceMask failOp passOp depthFailOp compareOp = liftIO $ do
  let vkCmdSetStencilOpPtr = pVkCmdSetStencilOp (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetStencilOpPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilOp is null" Nothing Nothing
  let vkCmdSetStencilOp' = mkVkCmdSetStencilOp vkCmdSetStencilOpPtr
  traceAroundEvent "vkCmdSetStencilOp" (vkCmdSetStencilOp' (commandBufferHandle (commandBuffer)) (faceMask) (failOp) (passOp) (depthFailOp) (compareOp))
  pure $ ()

