{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_extended_dynamic_state2"
module Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2  ( cmdSetRasterizerDiscardEnable
                                                                   , cmdSetDepthBiasEnable
                                                                   , cmdSetPrimitiveRestartEnable
                                                                   , DynamicState(..)
                                                                   ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBiasEnable))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPrimitiveRestartEnable))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRasterizerDiscardEnable))
import Vulkan.Core10.Enums.DynamicState (DynamicState(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRasterizerDiscardEnable
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetRasterizerDiscardEnable - Control whether primitives are
-- discarded before the rasterization stage dynamically for a command
-- buffer
--
-- = Description
--
-- This command sets the discard enable for subsequent drawing commands
-- when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@rasterizerDiscardEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetRasterizerDiscardEnable-None-08970# At least one of
--     the following /must/ be true:
--
--     -   the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState2 extendedDynamicState2>
--         feature is enabled
--
--     -   the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
--     -   the value of
--         'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--         used to create the 'Vulkan.Core10.Handles.Instance' parent of
--         @commandBuffer@ is greater than or equal to Version 1.3
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRasterizerDiscardEnable-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRasterizerDiscardEnable-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRasterizerDiscardEnable-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetRasterizerDiscardEnable-videocoding# This command
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state2 VK_EXT_extended_dynamic_state2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetRasterizerDiscardEnable :: forall io
                               . (MonadIO io)
                              => -- | @commandBuffer@ is the command buffer into which the command will be
                                 -- recorded.
                                 CommandBuffer
                              -> -- | @rasterizerDiscardEnable@ controls whether primitives are discarded
                                 -- immediately before the rasterization stage.
                                 ("rasterizerDiscardEnable" ::: Bool)
                              -> io ()
cmdSetRasterizerDiscardEnable commandBuffer
                                rasterizerDiscardEnable = liftIO $ do
  let vkCmdSetRasterizerDiscardEnablePtr = pVkCmdSetRasterizerDiscardEnable (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetRasterizerDiscardEnablePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRasterizerDiscardEnable is null" Nothing Nothing
  let vkCmdSetRasterizerDiscardEnable' = mkVkCmdSetRasterizerDiscardEnable vkCmdSetRasterizerDiscardEnablePtr
  traceAroundEvent "vkCmdSetRasterizerDiscardEnable" (vkCmdSetRasterizerDiscardEnable'
                                                        (commandBufferHandle (commandBuffer))
                                                        (boolToBool32 (rasterizerDiscardEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBiasEnable
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthBiasEnable - Control whether to bias fragment depth values
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the depth bias enable for subsequent drawing commands
-- when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@depthBiasEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthBiasEnable-None-08970# At least one of the
--     following /must/ be true:
--
--     -   the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState2 extendedDynamicState2>
--         feature is enabled
--
--     -   the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
--     -   the value of
--         'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--         used to create the 'Vulkan.Core10.Handles.Instance' parent of
--         @commandBuffer@ is greater than or equal to Version 1.3
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthBiasEnable-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthBiasEnable-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthBiasEnable-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthBiasEnable-videocoding# This command /must/ only
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state2 VK_EXT_extended_dynamic_state2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthBiasEnable :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @depthBiasEnable@ controls whether to bias fragment depth values.
                         ("depthBiasEnable" ::: Bool)
                      -> io ()
cmdSetDepthBiasEnable commandBuffer depthBiasEnable = liftIO $ do
  let vkCmdSetDepthBiasEnablePtr = pVkCmdSetDepthBiasEnable (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDepthBiasEnablePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBiasEnable is null" Nothing Nothing
  let vkCmdSetDepthBiasEnable' = mkVkCmdSetDepthBiasEnable vkCmdSetDepthBiasEnablePtr
  traceAroundEvent "vkCmdSetDepthBiasEnable" (vkCmdSetDepthBiasEnable'
                                                (commandBufferHandle (commandBuffer))
                                                (boolToBool32 (depthBiasEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPrimitiveRestartEnable
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetPrimitiveRestartEnable - Set primitive assembly restart state
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the primitive restart enable for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@primitiveRestartEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetPrimitiveRestartEnable-None-08970# At least one of the
--     following /must/ be true:
--
--     -   the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState2 extendedDynamicState2>
--         feature is enabled
--
--     -   the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
--     -   the value of
--         'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--         used to create the 'Vulkan.Core10.Handles.Instance' parent of
--         @commandBuffer@ is greater than or equal to Version 1.3
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetPrimitiveRestartEnable-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetPrimitiveRestartEnable-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetPrimitiveRestartEnable-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetPrimitiveRestartEnable-videocoding# This command
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state2 VK_EXT_extended_dynamic_state2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetPrimitiveRestartEnable :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer into which the command will be
                                -- recorded.
                                CommandBuffer
                             -> -- | @primitiveRestartEnable@ controls whether a special vertex index value
                                -- is treated as restarting the assembly of primitives. It behaves in the
                                -- same way as
                                -- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@primitiveRestartEnable@
                                ("primitiveRestartEnable" ::: Bool)
                             -> io ()
cmdSetPrimitiveRestartEnable commandBuffer primitiveRestartEnable = liftIO $ do
  let vkCmdSetPrimitiveRestartEnablePtr = pVkCmdSetPrimitiveRestartEnable (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetPrimitiveRestartEnablePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPrimitiveRestartEnable is null" Nothing Nothing
  let vkCmdSetPrimitiveRestartEnable' = mkVkCmdSetPrimitiveRestartEnable vkCmdSetPrimitiveRestartEnablePtr
  traceAroundEvent "vkCmdSetPrimitiveRestartEnable" (vkCmdSetPrimitiveRestartEnable'
                                                       (commandBufferHandle (commandBuffer))
                                                       (boolToBool32 (primitiveRestartEnable)))
  pure $ ()

