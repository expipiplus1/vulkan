{-# language CPP #-}
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

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
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
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.Pipeline (Viewport)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCullModeEXT
  :: FunPtr (Ptr CommandBuffer_T -> CullModeFlags -> IO ()) -> Ptr CommandBuffer_T -> CullModeFlags -> IO ()

-- | vkCmdSetCullModeEXT - Set the cull mode property
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @cullMode@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlagBits' values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  vkCmdSetCullModeEXT' (commandBufferHandle (commandBuffer)) (cullMode)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetFrontFaceEXT
  :: FunPtr (Ptr CommandBuffer_T -> FrontFace -> IO ()) -> Ptr CommandBuffer_T -> FrontFace -> IO ()

-- | vkCmdSetFrontFaceEXT - Set the front face property
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @frontFace@ /must/ be a valid
--     'Vulkan.Core10.Enums.FrontFace.FrontFace' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.FrontFace.FrontFace'
cmdSetFrontFaceEXT :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command will be
                      -- recorded.
                      CommandBuffer
                   -> -- | @frontFace@ specifies the front face property to use for drawing.
                      FrontFace
                   -> io ()
cmdSetFrontFaceEXT commandBuffer frontFace = liftIO $ do
  let vkCmdSetFrontFaceEXTPtr = pVkCmdSetFrontFaceEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetFrontFaceEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetFrontFaceEXT is null" Nothing Nothing
  let vkCmdSetFrontFaceEXT' = mkVkCmdSetFrontFaceEXT vkCmdSetFrontFaceEXTPtr
  vkCmdSetFrontFaceEXT' (commandBufferHandle (commandBuffer)) (frontFace)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPrimitiveTopologyEXT
  :: FunPtr (Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()) -> Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()

-- | vkCmdSetPrimitiveTopologyEXT - Set the primitive topology state
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @primitiveTopology@ /must/ be a valid
--     'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology'
cmdSetPrimitiveTopologyEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command will be
                              -- recorded.
                              CommandBuffer
                           -> -- No documentation found for Nested "vkCmdSetPrimitiveTopologyEXT" "primitiveTopology"
                              PrimitiveTopology
                           -> io ()
cmdSetPrimitiveTopologyEXT commandBuffer primitiveTopology = liftIO $ do
  let vkCmdSetPrimitiveTopologyEXTPtr = pVkCmdSetPrimitiveTopologyEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetPrimitiveTopologyEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPrimitiveTopologyEXT is null" Nothing Nothing
  let vkCmdSetPrimitiveTopologyEXT' = mkVkCmdSetPrimitiveTopologyEXT vkCmdSetPrimitiveTopologyEXTPtr
  vkCmdSetPrimitiveTopologyEXT' (commandBufferHandle (commandBuffer)) (primitiveTopology)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportWithCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Viewport -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Viewport -> IO ()

-- | vkCmdSetViewportWithCountEXT - Set the viewport count and viewports
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- -   @viewportCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pViewports@ /must/ be a valid pointer to an array of
--     @viewportCount@ valid 'Vulkan.Core10.Pipeline.Viewport' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   @viewportCount@ /must/ be greater than @0@
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  pPViewports <- ContT $ allocaBytesAligned @Viewport ((Data.Vector.length (viewports)) * 24) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e) . ($ ())) (viewports)
  lift $ vkCmdSetViewportWithCountEXT' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (viewports)) :: Word32)) (pPViewports)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissorWithCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetScissorWithCountEXT - Set the scissor count and scissors
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- -   @scissorCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   The @x@ and @y@ members of @offset@ member of any element of
--     @pScissors@ /must/ be greater than or equal to @0@
--
-- -   Evaluation of (@offset.x@ + @extent.width@) /must/ not cause a
--     signed integer addition overflow for any element of @pScissors@
--
-- -   Evaluation of (@offset.y@ + @extent.height@) /must/ not cause a
--     signed integer addition overflow for any element of @pScissors@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pScissors@ /must/ be a valid pointer to an array of @scissorCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   @scissorCount@ /must/ be greater than @0@
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  pPScissors <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (scissors)) * 16) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e) . ($ ())) (scissors)
  lift $ vkCmdSetScissorWithCountEXT' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (scissors)) :: Word32)) (pPScissors)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindVertexBuffers2EXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()

-- | vkCmdBindVertexBuffers2EXT - Bind vertex buffers to a command buffer
--
-- = Description
--
-- The values taken from elements i of @pBuffers@ and @pOffsets@ replace
-- the current state for the vertex input binding @firstBinding@ + i, for i
-- in [0, @bindingCount@). The vertex input binding is updated to start at
-- the offset indicated by @pOffsets@[i] from the start of the buffer
-- @pBuffers@[i] If @pSizes@ is not @NULL@ then @pSizes@[i] specifies the
-- bound size of the corresponding vertex buffer. All vertex input
-- attributes that use each of these bindings will use these updated
-- addresses in their address calculations for subsequent draw commands.
--
-- If the bound pipeline state object was created with the
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
-- dynamic state enabled then @pStrides@[i] specifies the distance in bytes
-- between two consecutive elements within the corresponding buffer. In
-- this case the
-- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription'::@stride@ state
-- from the pipeline state object is ignored.
--
-- == Valid Usage
--
-- -   @firstBinding@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   The sum of @firstBinding@ and @bindingCount@ /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   All elements of @pOffsets@ /must/ be less than the size of the
--     corresponding element in @pBuffers@
--
-- -   If @pSizes@ is not @NULL@, all elements of @pOffsets@ plus @pSizes@
--     /must/ be less than or equal to the size of the corresponding
--     element in @pBuffers@
--
-- -   All elements of @pBuffers@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     flag
--
-- -   Each element of @pBuffers@ that is non-sparse /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all elements of @pBuffers@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If an element of @pBuffers@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', then the corresponding
--     element of @pOffsets@ /must/ be zero
--
-- -   If the bound pipeline state object was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic state enabled then @pStrides@ /must/ not be @NULL@,
--     otherwise @pStrides@ /must/ be @NULL@
--
-- -   If @pStrides@ is not @NULL@ each element of @pStrides@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindingStride@
--
-- -   If @pStrides@ is not @NULL@ each element of @pStrides@ /must/ be
--     greater than or equal to the maximum extent of of all vertex input
--     attributes fetched from the corresponding binding, where the extent
--     is calculated as the VkVertexInputAttributeDescription::offset plus
--     VkVertexInputAttributeDescription::format size
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pBuffers@ /must/ be a valid pointer to an array of @bindingCount@
--     valid 'Vulkan.Core10.Handles.Buffer' handles
--
-- -   @pOffsets@ /must/ be a valid pointer to an array of @bindingCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   If @pSizes@ is not @NULL@, @pSizes@ /must/ be a valid pointer to an
--     array of @bindingCount@ 'Vulkan.Core10.FundamentalTypes.DeviceSize'
--     values
--
-- -   If @pStrides@ is not @NULL@, @pStrides@ /must/ be a valid pointer to
--     an array of @bindingCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   If any of @pSizes@, or @pStrides@ are not @NULL@, @bindingCount@
--     /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pBuffers@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
                         -> -- | @pSizes@ is optional, and when not @NULL@ is a pointer to an array of
                            -- buffer sizes.
                            ("sizes" ::: Vector DeviceSize)
                         -> -- | @pStrides@ is optional, and when not @NULL@ is a pointer to an array of
                            -- buffer strides.
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
  pPBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (buffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytesAligned @DeviceSize ((Data.Vector.length (offsets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  pSizes <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pPSizes <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (sizes))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pPSizes
  pStrides <- if Data.Vector.null (strides)
    then pure nullPtr
    else do
      pPStrides <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (strides))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPStrides `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((strides))
      pure $ pPStrides
  lift $ vkCmdBindVertexBuffers2EXT' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets) pSizes pStrides
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthTestEnableEXT - Set the depth test enable for a command
-- buffer
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  vkCmdSetDepthTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthTestEnable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthWriteEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthWriteEnableEXT - Set the depth write enable for the command
-- buffer
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  vkCmdSetDepthWriteEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthWriteEnable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthCompareOpEXT
  :: FunPtr (Ptr CommandBuffer_T -> CompareOp -> IO ()) -> Ptr CommandBuffer_T -> CompareOp -> IO ()

-- | vkCmdSetDepthCompareOpEXT - Set the depth comparison operator for the
-- command buffer
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @depthCompareOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  vkCmdSetDepthCompareOpEXT' (commandBufferHandle (commandBuffer)) (depthCompareOp)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBoundsTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthBoundsTestEnableEXT - Set the depth bounds test enable for
-- a command buffer
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  vkCmdSetDepthBoundsTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthBoundsTestEnable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetStencilTestEnableEXT - Set the stencil test enable for the
-- command buffer
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  vkCmdSetStencilTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (stencilTestEnable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilOpEXT
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> StencilOp -> StencilOp -> StencilOp -> CompareOp -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> StencilOp -> StencilOp -> StencilOp -> CompareOp -> IO ()

-- | vkCmdSetStencilOpEXT - Set the stencil operation for the command buffer
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP_EXT' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @faceMask@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   @failOp@ /must/ be a valid 'Vulkan.Core10.Enums.StencilOp.StencilOp'
--     value
--
-- -   @passOp@ /must/ be a valid 'Vulkan.Core10.Enums.StencilOp.StencilOp'
--     value
--
-- -   @depthFailOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.StencilOp.StencilOp' value
--
-- -   @compareOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags',
-- 'Vulkan.Core10.Enums.StencilOp.StencilOp'
cmdSetStencilOpEXT :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command will be
                      -- recorded.
                      CommandBuffer
                   -> -- | @faceMask@ is a bitmask of VkStencilFaceFlagBits specifying the set of
                      -- stencil state for which to update the stencil operation.
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
  vkCmdSetStencilOpEXT' (commandBufferHandle (commandBuffer)) (faceMask) (failOp) (passOp) (depthFailOp) (compareOp)
  pure $ ()


-- | VkPhysicalDeviceExtendedDynamicStateFeaturesEXT - Structure describing
-- what extended dynamic state can be used
--
-- = Members
--
-- The members of the 'PhysicalDeviceExtendedDynamicStateFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceExtendedDynamicStateFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceExtendedDynamicStateFeaturesEXT' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExtendedDynamicStateFeaturesEXT = PhysicalDeviceExtendedDynamicStateFeaturesEXT
  { -- | @extendedDynamicState@ indicates that the implementation supports the
    -- following dynamic states:
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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

