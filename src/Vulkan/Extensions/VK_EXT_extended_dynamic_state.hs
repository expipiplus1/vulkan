{-# language CPP #-}
-- | = Name
--
-- VK_EXT_extended_dynamic_state - device extension
--
-- == VK_EXT_extended_dynamic_state
--
-- [__Name String__]
--     @VK_EXT_extended_dynamic_state@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     268
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_extended_dynamic_state] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_EXT_extended_dynamic_state extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-12-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Dan Ginsburg, Valve Corporation
--
--     -   Graeme Leese, Broadcom
--
--     -   Hans-Kristian Arntzen, Valve Corporation
--
--     -   Jan-Harald Fredriksen, Arm Limited
--
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Hall, Google
--
--     -   Philip Rebohle, Valve Corporation
--
--     -   Stuart Smith, Imagination Technologies
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension adds some more dynamic state to support applications that
-- need to reduce the number of pipeline state objects they compile and
-- bind.
--
-- == New Commands
--
-- -   'cmdBindVertexBuffers2EXT'
--
-- -   'cmdSetCullModeEXT'
--
-- -   'cmdSetDepthBoundsTestEnableEXT'
--
-- -   'cmdSetDepthCompareOpEXT'
--
-- -   'cmdSetDepthTestEnableEXT'
--
-- -   'cmdSetDepthWriteEnableEXT'
--
-- -   'cmdSetFrontFaceEXT'
--
-- -   'cmdSetPrimitiveTopologyEXT'
--
-- -   'cmdSetScissorWithCountEXT'
--
-- -   'cmdSetStencilOpEXT'
--
-- -   'cmdSetStencilTestEnableEXT'
--
-- -   'cmdSetViewportWithCountEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExtendedDynamicStateFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME'
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-12-09 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceExtendedDynamicStateFeaturesEXT',
-- 'cmdBindVertexBuffers2EXT', 'cmdSetCullModeEXT',
-- 'cmdSetDepthBoundsTestEnableEXT', 'cmdSetDepthCompareOpEXT',
-- 'cmdSetDepthTestEnableEXT', 'cmdSetDepthWriteEnableEXT',
-- 'cmdSetFrontFaceEXT', 'cmdSetPrimitiveTopologyEXT',
-- 'cmdSetScissorWithCountEXT', 'cmdSetStencilOpEXT',
-- 'cmdSetStencilTestEnableEXT', 'cmdSetViewportWithCountEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_extended_dynamic_state  ( cmdSetCullModeEXT
                                                        , cmdSetFrontFaceEXT
                                                        , cmdSetPrimitiveTopologyEXT
                                                        , cmdSetViewportWithCountEXT
                                                        , cmdSetScissorWithCountEXT
                                                        , cmdBindVertexBuffers2EXT
                                                        , cmdSetDepthTestEnableEXT
                                                        , cmdSetDepthWriteEnableEXT
                                                        , cmdSetDepthCompareOpEXT
                                                        , cmdSetDepthBoundsTestEnableEXT
                                                        , cmdSetStencilTestEnableEXT
                                                        , cmdSetStencilOpEXT
                                                        , PhysicalDeviceExtendedDynamicStateFeaturesEXT(..)
                                                        , EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION
                                                        , pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION
                                                        , EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
                                                        , pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.CompareOp (CompareOp)
import Vulkan.Core10.Enums.CompareOp (CompareOp(..))
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlagBits(..))
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlags)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindVertexBuffers2EXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCullModeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBoundsTestEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthCompareOpEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthTestEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthWriteEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetFrontFaceEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPrimitiveTopologyEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetScissorWithCountEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilOpEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilTestEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportWithCountEXT))
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Pipeline (Viewport)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCullModeEXT
  :: FunPtr (Ptr CommandBuffer_T -> CullModeFlags -> IO ()) -> Ptr CommandBuffer_T -> CullModeFlags -> IO ()

-- | vkCmdSetCullModeEXT - Set cull mode dynamically for a command buffer
--
-- = Description
--
-- This command sets the cull mode for subsequent drawing commands when the
-- graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE_EXT' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@cullMode@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetCullModeEXT-None-03384# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCullModeEXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCullModeEXT-cullMode-parameter# @cullMode@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlagBits' values
--
-- -   #VUID-vkCmdSetCullModeEXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCullModeEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlags'
cmdSetCullModeEXT :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command will be
                     -- recorded.
                     CommandBuffer
                  -> -- | @cullMode@ specifies the cull mode property to use for drawing.
                     CullModeFlags
                  -> io ()
cmdSetCullModeEXT commandBuffer cullMode = liftIO $ do
  let vkCmdSetCullModeEXTPtr = pVkCmdSetCullModeEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetCullModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCullModeEXT is null" Nothing Nothing
  let vkCmdSetCullModeEXT' = mkVkCmdSetCullModeEXT vkCmdSetCullModeEXTPtr
  traceAroundEvent "vkCmdSetCullModeEXT" (vkCmdSetCullModeEXT' (commandBufferHandle (commandBuffer)) (cullMode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetFrontFaceEXT
  :: FunPtr (Ptr CommandBuffer_T -> FrontFace -> IO ()) -> Ptr CommandBuffer_T -> FrontFace -> IO ()

-- | vkCmdSetFrontFaceEXT - Set front face orientation dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the front face orientation for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE_EXT' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@frontFace@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetFrontFaceEXT-None-03383# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetFrontFaceEXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetFrontFaceEXT-frontFace-parameter# @frontFace@ /must/
--     be a valid 'Vulkan.Core10.Enums.FrontFace.FrontFace' value
--
-- -   #VUID-vkCmdSetFrontFaceEXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetFrontFaceEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.FrontFace.FrontFace'
cmdSetFrontFaceEXT :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command will be
                      -- recorded.
                      CommandBuffer
                   -> -- | @frontFace@ is a 'Vulkan.Core10.Enums.FrontFace.FrontFace' value
                      -- specifying the front-facing triangle orientation to be used for culling.
                      FrontFace
                   -> io ()
cmdSetFrontFaceEXT commandBuffer frontFace = liftIO $ do
  let vkCmdSetFrontFaceEXTPtr = pVkCmdSetFrontFaceEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetFrontFaceEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetFrontFaceEXT is null" Nothing Nothing
  let vkCmdSetFrontFaceEXT' = mkVkCmdSetFrontFaceEXT vkCmdSetFrontFaceEXTPtr
  traceAroundEvent "vkCmdSetFrontFaceEXT" (vkCmdSetFrontFaceEXT' (commandBufferHandle (commandBuffer)) (frontFace))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPrimitiveTopologyEXT
  :: FunPtr (Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()) -> Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()

-- | vkCmdSetPrimitiveTopologyEXT - Set primitive topology state dynamically
-- for a command buffer
--
-- = Description
--
-- This command sets the primitive topology for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetPrimitiveTopologyEXT-None-03347# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetPrimitiveTopologyEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetPrimitiveTopologyEXT-primitiveTopology-parameter#
--     @primitiveTopology@ /must/ be a valid
--     'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology' value
--
-- -   #VUID-vkCmdSetPrimitiveTopologyEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetPrimitiveTopologyEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology'
cmdSetPrimitiveTopologyEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command will be
                              -- recorded.
                              CommandBuffer
                           -> -- | @primitiveTopology@ specifies the primitive topology to use for drawing.
                              PrimitiveTopology
                           -> io ()
cmdSetPrimitiveTopologyEXT commandBuffer primitiveTopology = liftIO $ do
  let vkCmdSetPrimitiveTopologyEXTPtr = pVkCmdSetPrimitiveTopologyEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetPrimitiveTopologyEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPrimitiveTopologyEXT is null" Nothing Nothing
  let vkCmdSetPrimitiveTopologyEXT' = mkVkCmdSetPrimitiveTopologyEXT vkCmdSetPrimitiveTopologyEXTPtr
  traceAroundEvent "vkCmdSetPrimitiveTopologyEXT" (vkCmdSetPrimitiveTopologyEXT' (commandBufferHandle (commandBuffer)) (primitiveTopology))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportWithCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Viewport -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Viewport -> IO ()

-- | vkCmdSetViewportWithCountEXT - Set the viewport count and viewports
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the viewport count and viewports state for subsequent
-- drawing commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the corresponding
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
-- and @pViewports@ values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-None-03393# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-viewportCount-03394#
--     @viewportCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-viewportCount-03395# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-commandBuffer-04819#
--     @commandBuffer@ /must/ not have
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV'::@viewportScissor2D@
--     enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-pViewports-parameter#
--     @pViewports@ /must/ be a valid pointer to an array of
--     @viewportCount@ valid 'Vulkan.Core10.Pipeline.Viewport' structures
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetViewportWithCountEXT-viewportCount-arraylength#
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Pipeline.Viewport'
cmdSetViewportWithCountEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command will be
                              -- recorded.
                              CommandBuffer
                           -> -- | @pViewports@ specifies the viewports to use for drawing.
                              ("viewports" ::: Vector Viewport)
                           -> io ()
cmdSetViewportWithCountEXT commandBuffer viewports = liftIO . evalContT $ do
  let vkCmdSetViewportWithCountEXTPtr = pVkCmdSetViewportWithCountEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetViewportWithCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportWithCountEXT is null" Nothing Nothing
  let vkCmdSetViewportWithCountEXT' = mkVkCmdSetViewportWithCountEXT vkCmdSetViewportWithCountEXTPtr
  pPViewports <- ContT $ allocaBytes @Viewport ((Data.Vector.length (viewports)) * 24)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e)) (viewports)
  lift $ traceAroundEvent "vkCmdSetViewportWithCountEXT" (vkCmdSetViewportWithCountEXT' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (viewports)) :: Word32)) (pPViewports))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissorWithCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetScissorWithCountEXT - Set the scissor count and scissor
-- rectangular bounds dynamically for a command buffer
--
-- = Description
--
-- This command sets the scissor count and scissor rectangular bounds state
-- for subsequence drawing commands when the graphics pipeline is created
-- with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the corresponding
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
-- and @pScissors@ values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-None-03396# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-scissorCount-03397# @scissorCount@
--     /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-scissorCount-03398# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-x-03399# The @x@ and @y@ members
--     of @offset@ member of any element of @pScissors@ /must/ be greater
--     than or equal to @0@
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-offset-03400# Evaluation of
--     (@offset.x@ + @extent.width@) /must/ not cause a signed integer
--     addition overflow for any element of @pScissors@
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-offset-03401# Evaluation of
--     (@offset.y@ + @extent.height@) /must/ not cause a signed integer
--     addition overflow for any element of @pScissors@
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-commandBuffer-04820#
--     @commandBuffer@ /must/ not have
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV'::@viewportScissor2D@
--     enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-pScissors-parameter# @pScissors@
--     /must/ be a valid pointer to an array of @scissorCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetScissorWithCountEXT-scissorCount-arraylength#
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D'
cmdSetScissorWithCountEXT :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @pScissors@ specifies the scissors to use for drawing.
                             ("scissors" ::: Vector Rect2D)
                          -> io ()
cmdSetScissorWithCountEXT commandBuffer scissors = liftIO . evalContT $ do
  let vkCmdSetScissorWithCountEXTPtr = pVkCmdSetScissorWithCountEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetScissorWithCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetScissorWithCountEXT is null" Nothing Nothing
  let vkCmdSetScissorWithCountEXT' = mkVkCmdSetScissorWithCountEXT vkCmdSetScissorWithCountEXTPtr
  pPScissors <- ContT $ allocaBytes @Rect2D ((Data.Vector.length (scissors)) * 16)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (scissors)
  lift $ traceAroundEvent "vkCmdSetScissorWithCountEXT" (vkCmdSetScissorWithCountEXT' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (scissors)) :: Word32)) (pPScissors))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindVertexBuffers2EXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()

-- | vkCmdBindVertexBuffers2EXT - Bind vertex buffers to a command buffer and
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
-- feature is enabled, elements of @pBuffers@ /can/ be
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', and /can/ be used by the
-- vertex shader. If a vertex input attribute is bound to a vertex input
-- binding that is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the values
-- taken from memory are considered to be zero, and missing G, B, or A
-- components are
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input-extraction filled with (0,0,1)>.
--
-- This command also \<pipelines-dynamic-state, dynamically sets>> the byte
-- strides between consecutive elements within buffer @pBuffers@[i] to the
-- corresponding @pStrides@[i] value when the graphics pipeline is created
-- with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
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
-- /can/ be used instead of 'cmdBindVertexBuffers2EXT' to set the stride.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-firstBinding-03355# @firstBinding@
--     /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-firstBinding-03356# The sum of
--     @firstBinding@ and @bindingCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pOffsets-03357# All elements of
--     @pOffsets@ /must/ be less than the size of the corresponding element
--     in @pBuffers@
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pSizes-03358# If @pSizes@ is not
--     @NULL@, all elements of @pOffsets@ plus @pSizes@ /must/ be less than
--     or equal to the size of the corresponding element in @pBuffers@
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pBuffers-03359# All elements of
--     @pBuffers@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pBuffers-03360# Each element of
--     @pBuffers@ that is non-sparse /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pBuffers-04111# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all elements of @pBuffers@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pBuffers-04112# If an element of
--     @pBuffers@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     corresponding element of @pOffsets@ /must/ be zero
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pStrides-03362# If @pStrides@ is
--     not @NULL@ each element of @pStrides@ /must/ be less than or equal
--     to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindingStride@
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pStrides-06209# If @pStrides@ is
--     not @NULL@ each element of @pStrides@ /must/ be either 0 or greater
--     than or equal to the maximum extent of all vertex input attributes
--     fetched from the corresponding binding, where the extent is
--     calculated as the
--     'Vulkan.Core10.Pipeline.VertexInputAttributeDescription'::@offset@
--     plus
--     'Vulkan.Core10.Pipeline.VertexInputAttributeDescription'::@format@
--     size
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pBuffers-parameter# @pBuffers@
--     /must/ be a valid pointer to an array of @bindingCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Core10.Handles.Buffer' handles
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pOffsets-parameter# @pOffsets@
--     /must/ be a valid pointer to an array of @bindingCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pSizes-parameter# If @pSizes@ is
--     not @NULL@, @pSizes@ /must/ be a valid pointer to an array of
--     @bindingCount@ 'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-pStrides-parameter# If @pStrides@
--     is not @NULL@, @pStrides@ /must/ be a valid pointer to an array of
--     @bindingCount@ 'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-bindingCount-arraylength# If any of
--     @pSizes@, or @pStrides@ are not @NULL@, @bindingCount@ /must/ be
--     greater than @0@
--
-- -   #VUID-vkCmdBindVertexBuffers2EXT-commonparent# Both of
--     @commandBuffer@, and the elements of @pBuffers@ that are valid
--     handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdBindVertexBuffers2EXT :: forall io
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
cmdBindVertexBuffers2EXT commandBuffer firstBinding buffers offsets sizes strides = liftIO . evalContT $ do
  let vkCmdBindVertexBuffers2EXTPtr = pVkCmdBindVertexBuffers2EXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindVertexBuffers2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindVertexBuffers2EXT is null" Nothing Nothing
  let vkCmdBindVertexBuffers2EXT' = mkVkCmdBindVertexBuffers2EXT vkCmdBindVertexBuffers2EXTPtr
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
  lift $ traceAroundEvent "vkCmdBindVertexBuffers2EXT" (vkCmdBindVertexBuffers2EXT' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets) pSizes pStrides)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthTestEnableEXT - Set depth test enable dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the depth test enable for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthTestEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthTestEnableEXT-None-03352# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthTestEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthTestEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthTestEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthTestEnableEXT :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @depthTestEnable@ specifies if the depth test is enabled.
                            ("depthTestEnable" ::: Bool)
                         -> io ()
cmdSetDepthTestEnableEXT commandBuffer depthTestEnable = liftIO $ do
  let vkCmdSetDepthTestEnableEXTPtr = pVkCmdSetDepthTestEnableEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthTestEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthTestEnableEXT is null" Nothing Nothing
  let vkCmdSetDepthTestEnableEXT' = mkVkCmdSetDepthTestEnableEXT vkCmdSetDepthTestEnableEXTPtr
  traceAroundEvent "vkCmdSetDepthTestEnableEXT" (vkCmdSetDepthTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthTestEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthWriteEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthWriteEnableEXT - Set depth write enable dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the depth write enable for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthWriteEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthWriteEnableEXT-None-03354# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthWriteEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthWriteEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthWriteEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthWriteEnableEXT :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @depthWriteEnable@ specifies if depth writes are enabled.
                             ("depthWriteEnable" ::: Bool)
                          -> io ()
cmdSetDepthWriteEnableEXT commandBuffer depthWriteEnable = liftIO $ do
  let vkCmdSetDepthWriteEnableEXTPtr = pVkCmdSetDepthWriteEnableEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthWriteEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthWriteEnableEXT is null" Nothing Nothing
  let vkCmdSetDepthWriteEnableEXT' = mkVkCmdSetDepthWriteEnableEXT vkCmdSetDepthWriteEnableEXTPtr
  traceAroundEvent "vkCmdSetDepthWriteEnableEXT" (vkCmdSetDepthWriteEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthWriteEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthCompareOpEXT
  :: FunPtr (Ptr CommandBuffer_T -> CompareOp -> IO ()) -> Ptr CommandBuffer_T -> CompareOp -> IO ()

-- | vkCmdSetDepthCompareOpEXT - Set depth comparison operator dynamically
-- for a command buffer
--
-- = Description
--
-- This command sets the depth comparison operator for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthCompareOp@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthCompareOpEXT-None-03353# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthCompareOpEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthCompareOpEXT-depthCompareOp-parameter#
--     @depthCompareOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   #VUID-vkCmdSetDepthCompareOpEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthCompareOpEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp'
cmdSetDepthCompareOpEXT :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @depthCompareOp@ specifies the depth comparison operator.
                           ("depthCompareOp" ::: CompareOp)
                        -> io ()
cmdSetDepthCompareOpEXT commandBuffer depthCompareOp = liftIO $ do
  let vkCmdSetDepthCompareOpEXTPtr = pVkCmdSetDepthCompareOpEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthCompareOpEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthCompareOpEXT is null" Nothing Nothing
  let vkCmdSetDepthCompareOpEXT' = mkVkCmdSetDepthCompareOpEXT vkCmdSetDepthCompareOpEXTPtr
  traceAroundEvent "vkCmdSetDepthCompareOpEXT" (vkCmdSetDepthCompareOpEXT' (commandBufferHandle (commandBuffer)) (depthCompareOp))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBoundsTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthBoundsTestEnableEXT - Set depth bounds test enable
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the depth bounds enable for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthBoundsTestEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthBoundsTestEnableEXT-None-03349# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthBoundsTestEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthBoundsTestEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthBoundsTestEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthBoundsTestEnableEXT :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @depthBoundsTestEnable@ specifies if the depth bounds test is enabled.
                                  ("depthBoundsTestEnable" ::: Bool)
                               -> io ()
cmdSetDepthBoundsTestEnableEXT commandBuffer depthBoundsTestEnable = liftIO $ do
  let vkCmdSetDepthBoundsTestEnableEXTPtr = pVkCmdSetDepthBoundsTestEnableEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthBoundsTestEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBoundsTestEnableEXT is null" Nothing Nothing
  let vkCmdSetDepthBoundsTestEnableEXT' = mkVkCmdSetDepthBoundsTestEnableEXT vkCmdSetDepthBoundsTestEnableEXTPtr
  traceAroundEvent "vkCmdSetDepthBoundsTestEnableEXT" (vkCmdSetDepthBoundsTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthBoundsTestEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetStencilTestEnableEXT - Set stencil test enable dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the stencil test enable for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@stencilTestEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetStencilTestEnableEXT-None-03350# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetStencilTestEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetStencilTestEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetStencilTestEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetStencilTestEnableEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command will be
                              -- recorded.
                              CommandBuffer
                           -> -- | @stencilTestEnable@ specifies if the stencil test is enabled.
                              ("stencilTestEnable" ::: Bool)
                           -> io ()
cmdSetStencilTestEnableEXT commandBuffer stencilTestEnable = liftIO $ do
  let vkCmdSetStencilTestEnableEXTPtr = pVkCmdSetStencilTestEnableEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilTestEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilTestEnableEXT is null" Nothing Nothing
  let vkCmdSetStencilTestEnableEXT' = mkVkCmdSetStencilTestEnableEXT vkCmdSetStencilTestEnableEXTPtr
  traceAroundEvent "vkCmdSetStencilTestEnableEXT" (vkCmdSetStencilTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (stencilTestEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilOpEXT
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> StencilOp -> StencilOp -> StencilOp -> CompareOp -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> StencilOp -> StencilOp -> StencilOp -> CompareOp -> IO ()

-- | vkCmdSetStencilOpEXT - Set stencil operation dynamically for a command
-- buffer
--
-- = Description
--
-- This command sets the stencil operation for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP_EXT' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the corresponding
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@failOp@,
-- @passOp@, @depthFailOp@, and @compareOp@ values used to create the
-- currently active pipeline, for both front and back faces.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetStencilOpEXT-None-03351# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetStencilOpEXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetStencilOpEXT-faceMask-parameter# @faceMask@ /must/ be
--     a valid combination of
--     'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' values
--
-- -   #VUID-vkCmdSetStencilOpEXT-faceMask-requiredbitmask# @faceMask@
--     /must/ not be @0@
--
-- -   #VUID-vkCmdSetStencilOpEXT-failOp-parameter# @failOp@ /must/ be a
--     valid 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
--
-- -   #VUID-vkCmdSetStencilOpEXT-passOp-parameter# @passOp@ /must/ be a
--     valid 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
--
-- -   #VUID-vkCmdSetStencilOpEXT-depthFailOp-parameter# @depthFailOp@
--     /must/ be a valid 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
--
-- -   #VUID-vkCmdSetStencilOpEXT-compareOp-parameter# @compareOp@ /must/
--     be a valid 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   #VUID-vkCmdSetStencilOpEXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetStencilOpEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags',
-- 'Vulkan.Core10.Enums.StencilOp.StencilOp'
cmdSetStencilOpEXT :: forall io
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
cmdSetStencilOpEXT commandBuffer faceMask failOp passOp depthFailOp compareOp = liftIO $ do
  let vkCmdSetStencilOpEXTPtr = pVkCmdSetStencilOpEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilOpEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilOpEXT is null" Nothing Nothing
  let vkCmdSetStencilOpEXT' = mkVkCmdSetStencilOpEXT vkCmdSetStencilOpEXTPtr
  traceAroundEvent "vkCmdSetStencilOpEXT" (vkCmdSetStencilOpEXT' (commandBufferHandle (commandBuffer)) (faceMask) (failOp) (passOp) (depthFailOp) (compareOp))
  pure $ ()


-- | VkPhysicalDeviceExtendedDynamicStateFeaturesEXT - Structure describing
-- what extended dynamic state can be used
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceExtendedDynamicStateFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceExtendedDynamicStateFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExtendedDynamicStateFeaturesEXT = PhysicalDeviceExtendedDynamicStateFeaturesEXT
  { -- | #features-extendedDynamicState# @extendedDynamicState@ indicates that
    -- the implementation supports the following dynamic states:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP_EXT'
    extendedDynamicState :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExtendedDynamicStateFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceExtendedDynamicStateFeaturesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExtendedDynamicStateFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  peekCStruct p = do
    extendedDynamicState <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExtendedDynamicStateFeaturesEXT
             (bool32ToBool extendedDynamicState)

instance Storable PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  zero = PhysicalDeviceExtendedDynamicStateFeaturesEXT
           zero


type EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION"
pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1


type EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_extended_dynamic_state"

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME"
pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_extended_dynamic_state"

