{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_copy_commands2"
module Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2  ( cmdCopyBuffer2
                                                          , cmdCopyImage2
                                                          , cmdBlitImage2
                                                          , cmdCopyBufferToImage2
                                                          , cmdCopyImageToBuffer2
                                                          , cmdResolveImage2
                                                          , BufferCopy2(..)
                                                          , ImageCopy2(..)
                                                          , ImageBlit2(..)
                                                          , BufferImageCopy2(..)
                                                          , ImageResolve2(..)
                                                          , CopyBufferInfo2(..)
                                                          , CopyImageInfo2(..)
                                                          , BlitImageInfo2(..)
                                                          , CopyBufferToImageInfo2(..)
                                                          , CopyImageToBufferInfo2(..)
                                                          , ResolveImageInfo2(..)
                                                          , StructureType(..)
                                                          ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_filter_cubic_weights (BlitImageCubicWeightsInfoQCOM)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_rotated_copy_commands (CopyCommandTransformInfoQCOM)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBlitImage2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBuffer2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBufferToImage2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImage2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImageToBuffer2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResolveImage2))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Enums.Filter (Filter)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BLIT_IMAGE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COPY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_BUFFER_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_BLIT_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_COPY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_RESOLVE_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBuffer2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyBufferInfo2 -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyBufferInfo2 -> IO ()

-- | vkCmdCopyBuffer2 - Copy data between buffer regions
--
-- = Description
--
-- Each source region specified by @pCopyBufferInfo->pRegions@ is copied
-- from the source buffer to the destination region of the destination
-- buffer. If any of the specified regions in @pCopyBufferInfo->srcBuffer@
-- overlaps in memory with any of the specified regions in
-- @pCopyBufferInfo->dstBuffer@, values read from those overlapping regions
-- are undefined.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyBuffer2-commandBuffer-01822# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyBuffer2-commandBuffer-01823# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyBuffer2-commandBuffer-01824# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be an unprotected buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyBuffer2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyBuffer2-pCopyBufferInfo-parameter# @pCopyBufferInfo@
--     /must/ be a valid pointer to a valid 'CopyBufferInfo2' structure
--
-- -   #VUID-vkCmdCopyBuffer2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyBuffer2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyBuffer2-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyBuffer2-videocoding# This command /must/ only be
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyBufferInfo2'
cmdCopyBuffer2 :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer into which the command will be
                  -- recorded.
                  CommandBuffer
               -> -- | @pCopyBufferInfo@ is a pointer to a 'CopyBufferInfo2' structure
                  -- describing the copy parameters.
                  CopyBufferInfo2
               -> io ()
cmdCopyBuffer2 commandBuffer copyBufferInfo = liftIO . evalContT $ do
  let vkCmdCopyBuffer2Ptr = pVkCmdCopyBuffer2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyBuffer2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBuffer2 is null" Nothing Nothing
  let vkCmdCopyBuffer2' = mkVkCmdCopyBuffer2 vkCmdCopyBuffer2Ptr
  pCopyBufferInfo <- ContT $ withCStruct (copyBufferInfo)
  lift $ traceAroundEvent "vkCmdCopyBuffer2" (vkCmdCopyBuffer2'
                                                (commandBufferHandle (commandBuffer))
                                                pCopyBufferInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImage2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyImageInfo2 -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyImageInfo2 -> IO ()

-- | vkCmdCopyImage2 - Copy data between images
--
-- = Description
--
-- This command is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImage', but includes
-- extensible sub-structures that include @sType@ and @pNext@ parameters,
-- allowing them to be more easily extended.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyImage2-commandBuffer-01825# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyImage2-commandBuffer-01826# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyImage2-commandBuffer-01827# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be an unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyImage2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyImage2-pCopyImageInfo-parameter# @pCopyImageInfo@
--     /must/ be a valid pointer to a valid 'CopyImageInfo2' structure
--
-- -   #VUID-vkCmdCopyImage2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyImage2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyImage2-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdCopyImage2-videocoding# This command /must/ only be
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyImageInfo2'
cmdCopyImage2 :: forall io
               . (MonadIO io)
              => -- | @commandBuffer@ is the command buffer into which the command will be
                 -- recorded.
                 CommandBuffer
              -> -- | @pCopyImageInfo@ is a pointer to a 'CopyImageInfo2' structure describing
                 -- the copy parameters.
                 CopyImageInfo2
              -> io ()
cmdCopyImage2 commandBuffer copyImageInfo = liftIO . evalContT $ do
  let vkCmdCopyImage2Ptr = pVkCmdCopyImage2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyImage2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImage2 is null" Nothing Nothing
  let vkCmdCopyImage2' = mkVkCmdCopyImage2 vkCmdCopyImage2Ptr
  pCopyImageInfo <- ContT $ withCStruct (copyImageInfo)
  lift $ traceAroundEvent "vkCmdCopyImage2" (vkCmdCopyImage2'
                                               (commandBufferHandle (commandBuffer))
                                               pCopyImageInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBlitImage2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct BlitImageInfo2) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct BlitImageInfo2) -> IO ()

-- | vkCmdBlitImage2 - Copy regions of an image, potentially performing
-- format conversion,
--
-- = Description
--
-- This command is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage', but includes
-- extensible sub-structures that include @sType@ and @pNext@ parameters,
-- allowing them to be more easily extended.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBlitImage2-commandBuffer-01834# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdBlitImage2-commandBuffer-01835# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdBlitImage2-commandBuffer-01836# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be an unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBlitImage2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBlitImage2-pBlitImageInfo-parameter# @pBlitImageInfo@
--     /must/ be a valid pointer to a valid 'BlitImageInfo2' structure
--
-- -   #VUID-vkCmdBlitImage2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBlitImage2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBlitImage2-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdBlitImage2-videocoding# This command /must/ only be
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'BlitImageInfo2', 'Vulkan.Core10.Handles.CommandBuffer'
cmdBlitImage2 :: forall a io
               . (Extendss BlitImageInfo2 a, PokeChain a, MonadIO io)
              => -- | @commandBuffer@ is the command buffer into which the command will be
                 -- recorded.
                 CommandBuffer
              -> -- | @pBlitImageInfo@ is a pointer to a 'BlitImageInfo2' structure describing
                 -- the blit parameters.
                 (BlitImageInfo2 a)
              -> io ()
cmdBlitImage2 commandBuffer blitImageInfo = liftIO . evalContT $ do
  let vkCmdBlitImage2Ptr = pVkCmdBlitImage2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBlitImage2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBlitImage2 is null" Nothing Nothing
  let vkCmdBlitImage2' = mkVkCmdBlitImage2 vkCmdBlitImage2Ptr
  pBlitImageInfo <- ContT $ withCStruct (blitImageInfo)
  lift $ traceAroundEvent "vkCmdBlitImage2" (vkCmdBlitImage2'
                                               (commandBufferHandle (commandBuffer))
                                               (forgetExtensions pBlitImageInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBufferToImage2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyBufferToImageInfo2 -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyBufferToImageInfo2 -> IO ()

-- | vkCmdCopyBufferToImage2 - Copy data from a buffer into an image
--
-- = Description
--
-- This command is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage', but includes
-- extensible sub-structures that include @sType@ and @pNext@ parameters,
-- allowing them to be more easily extended.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyBufferToImage2-commandBuffer-01828# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyBufferToImage2-commandBuffer-01829# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyBufferToImage2-commandBuffer-01830# If
--     @commandBuffer@ is a protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be an unprotected image
--
-- -   #VUID-vkCmdCopyBufferToImage2-commandBuffer-07737# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', the
--     @bufferOffset@ member of any element of
--     @pCopyBufferToImageInfo->pRegions@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdCopyBufferToImage2-imageOffset-07738# The @imageOffset@
--     and @imageExtent@ members of each element of
--     @pCopyBufferToImageInfo->pRegions@ /must/ respect the image transfer
--     granularity requirements of @commandBuffer@’s command pool’s queue
--     family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-vkCmdCopyBufferToImage2-commandBuffer-07739# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', for each
--     element of @pCopyBufferToImageInfo->pRegions@, the @aspectMask@
--     member of @imageSubresource@ /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyBufferToImage2-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyBufferToImage2-pCopyBufferToImageInfo-parameter#
--     @pCopyBufferToImageInfo@ /must/ be a valid pointer to a valid
--     'CopyBufferToImageInfo2' structure
--
-- -   #VUID-vkCmdCopyBufferToImage2-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyBufferToImage2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyBufferToImage2-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyBufferToImage2-videocoding# This command /must/ only
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyBufferToImageInfo2'
cmdCopyBufferToImage2 :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @pCopyBufferToImageInfo@ is a pointer to a 'CopyBufferToImageInfo2'
                         -- structure describing the copy parameters.
                         CopyBufferToImageInfo2
                      -> io ()
cmdCopyBufferToImage2 commandBuffer
                        copyBufferToImageInfo = liftIO . evalContT $ do
  let vkCmdCopyBufferToImage2Ptr = pVkCmdCopyBufferToImage2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyBufferToImage2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBufferToImage2 is null" Nothing Nothing
  let vkCmdCopyBufferToImage2' = mkVkCmdCopyBufferToImage2 vkCmdCopyBufferToImage2Ptr
  pCopyBufferToImageInfo <- ContT $ withCStruct (copyBufferToImageInfo)
  lift $ traceAroundEvent "vkCmdCopyBufferToImage2" (vkCmdCopyBufferToImage2'
                                                       (commandBufferHandle (commandBuffer))
                                                       pCopyBufferToImageInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImageToBuffer2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyImageToBufferInfo2 -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyImageToBufferInfo2 -> IO ()

-- | vkCmdCopyImageToBuffer2 - Copy image data into a buffer
--
-- = Description
--
-- This command is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer', but includes
-- extensible sub-structures that include @sType@ and @pNext@ parameters,
-- allowing them to be more easily extended.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyImageToBuffer2-commandBuffer-01831# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyImageToBuffer2-commandBuffer-01832# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyImageToBuffer2-commandBuffer-01833# If
--     @commandBuffer@ is a protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be an unprotected buffer
--
-- -   #VUID-vkCmdCopyImageToBuffer2-commandBuffer-07746# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', the
--     @bufferOffset@ member of any element of
--     @pCopyImageToBufferInfo->pRegions@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdCopyImageToBuffer2-imageOffset-07747# The @imageOffset@
--     and @imageExtent@ members of each element of
--     @pCopyImageToBufferInfo->pRegions@ /must/ respect the image transfer
--     granularity requirements of @commandBuffer@’s command pool’s queue
--     family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyImageToBuffer2-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyImageToBuffer2-pCopyImageToBufferInfo-parameter#
--     @pCopyImageToBufferInfo@ /must/ be a valid pointer to a valid
--     'CopyImageToBufferInfo2' structure
--
-- -   #VUID-vkCmdCopyImageToBuffer2-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyImageToBuffer2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyImageToBuffer2-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyImageToBuffer2-videocoding# This command /must/ only
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyImageToBufferInfo2'
cmdCopyImageToBuffer2 :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @pCopyImageToBufferInfo@ is a pointer to a 'CopyImageToBufferInfo2'
                         -- structure describing the copy parameters.
                         CopyImageToBufferInfo2
                      -> io ()
cmdCopyImageToBuffer2 commandBuffer
                        copyImageToBufferInfo = liftIO . evalContT $ do
  let vkCmdCopyImageToBuffer2Ptr = pVkCmdCopyImageToBuffer2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyImageToBuffer2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImageToBuffer2 is null" Nothing Nothing
  let vkCmdCopyImageToBuffer2' = mkVkCmdCopyImageToBuffer2 vkCmdCopyImageToBuffer2Ptr
  pCopyImageToBufferInfo <- ContT $ withCStruct (copyImageToBufferInfo)
  lift $ traceAroundEvent "vkCmdCopyImageToBuffer2" (vkCmdCopyImageToBuffer2'
                                                       (commandBufferHandle (commandBuffer))
                                                       pCopyImageToBufferInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResolveImage2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr ResolveImageInfo2 -> IO ()) -> Ptr CommandBuffer_T -> Ptr ResolveImageInfo2 -> IO ()

-- | vkCmdResolveImage2 - Resolve regions of an image
--
-- = Description
--
-- This command is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage', but includes
-- extensible sub-structures that include @sType@ and @pNext@ parameters,
-- allowing them to be more easily extended.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdResolveImage2-commandBuffer-01837# If @commandBuffer@ is
--     an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdResolveImage2-commandBuffer-01838# If @commandBuffer@ is
--     an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdResolveImage2-commandBuffer-01839# If @commandBuffer@ is
--     a protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be an unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdResolveImage2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdResolveImage2-pResolveImageInfo-parameter#
--     @pResolveImageInfo@ /must/ be a valid pointer to a valid
--     'ResolveImageInfo2' structure
--
-- -   #VUID-vkCmdResolveImage2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdResolveImage2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdResolveImage2-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdResolveImage2-videocoding# This command /must/ only be
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'ResolveImageInfo2'
cmdResolveImage2 :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is the command buffer into which the command will be
                    -- recorded.
                    CommandBuffer
                 -> -- | @pResolveImageInfo@ is a pointer to a 'ResolveImageInfo2' structure
                    -- describing the resolve parameters.
                    ResolveImageInfo2
                 -> io ()
cmdResolveImage2 commandBuffer resolveImageInfo = liftIO . evalContT $ do
  let vkCmdResolveImage2Ptr = pVkCmdResolveImage2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdResolveImage2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResolveImage2 is null" Nothing Nothing
  let vkCmdResolveImage2' = mkVkCmdResolveImage2 vkCmdResolveImage2Ptr
  pResolveImageInfo <- ContT $ withCStruct (resolveImageInfo)
  lift $ traceAroundEvent "vkCmdResolveImage2" (vkCmdResolveImage2'
                                                  (commandBufferHandle (commandBuffer))
                                                  pResolveImageInfo)
  pure $ ()


-- | VkBufferCopy2 - Structure specifying a buffer copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkBufferCopy2-size-01988# The @size@ /must/ be greater than
--     @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferCopy2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COPY_2'
--
-- -   #VUID-VkBufferCopy2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'CopyBufferInfo2', 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferCopy2 = BufferCopy2
  { -- | @srcOffset@ is the starting offset in bytes from the start of
    -- @srcBuffer@.
    srcOffset :: DeviceSize
  , -- | @dstOffset@ is the starting offset in bytes from the start of
    -- @dstBuffer@.
    dstOffset :: DeviceSize
  , -- | @size@ is the number of bytes to copy.
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCopy2)
#endif
deriving instance Show BufferCopy2

instance ToCStruct BufferCopy2 where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCopy2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COPY_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (srcOffset)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (dstOffset)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COPY_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferCopy2 where
  peekCStruct p = do
    srcOffset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    dstOffset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ BufferCopy2
             srcOffset dstOffset size

instance Storable BufferCopy2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCopy2 where
  zero = BufferCopy2
           zero
           zero
           zero


-- | VkImageCopy2 - Structure specifying an image copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkImageCopy2-apiVersion-07940# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_sampler_ycbcr_conversion VK_KHR_sampler_ycbcr_conversion>
--     extension is not enabled, and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
--     is less than Vulkan 1.1, the @aspectMask@ member of @srcSubresource@
--     and @dstSubresource@ /must/ match
--
-- -   #VUID-VkImageCopy2-apiVersion-07941# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--     extension is not enabled, and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
--     is less than Vulkan 1.1, the @layerCount@ member of @srcSubresource@
--     and @dstSubresource@ /must/ match
--
-- -   #VUID-VkImageCopy2-extent-06668# @extent.width@ /must/ not be 0
--
-- -   #VUID-VkImageCopy2-extent-06669# @extent.height@ /must/ not be 0
--
-- -   #VUID-VkImageCopy2-extent-06670# @extent.depth@ /must/ not be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageCopy2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COPY_2'
--
-- -   #VUID-VkImageCopy2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkImageCopy2-srcSubresource-parameter# @srcSubresource@ /must/
--     be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- -   #VUID-VkImageCopy2-dstSubresource-parameter# @dstSubresource@ /must/
--     be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'CopyImageInfo2',
-- 'Vulkan.Extensions.VK_EXT_host_image_copy.CopyImageToImageInfoEXT',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageCopy2 = ImageCopy2
  { -- | @srcSubresource@ and @dstSubresource@ are
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' structures
    -- specifying the image subresources of the images used for the source and
    -- destination image data, respectively.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
    -- in texels of the sub-regions of the source and destination image data.
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageCopy2" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy2" "dstOffset"
    dstOffset :: Offset3D
  , -- | @extent@ is the size in texels of the image to copy in @width@, @height@
    -- and @depth@.
    extent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCopy2)
#endif
deriving instance Show ImageCopy2

instance ToCStruct ImageCopy2 where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCopy2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COPY_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (srcOffset)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (dstOffset)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (extent)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COPY_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageCopy2 where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 60 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 72 :: Ptr Extent3D))
    pure $ ImageCopy2
             srcSubresource srcOffset dstSubresource dstOffset extent

instance Storable ImageCopy2 where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCopy2 where
  zero = ImageCopy2
           zero
           zero
           zero
           zero
           zero


-- | VkImageBlit2 - Structure specifying an image blit operation
--
-- = Description
--
-- For each element of the @pRegions@ array, a blit operation is performed
-- for the specified source and destination regions.
--
-- == Valid Usage
--
-- -   #VUID-VkImageBlit2-aspectMask-00238# The @aspectMask@ member of
--     @srcSubresource@ and @dstSubresource@ /must/ match
--
-- -   #VUID-VkImageBlit2-layerCount-08800# If neither of the @layerCount@
--     members of @srcSubresource@ or @dstSubresource@ are
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the
--     @layerCount@ members of @srcSubresource@ or @dstSubresource@ /must/
--     match
--
-- -   #VUID-VkImageBlit2-maintenance5-08799# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     feature is not enabled, the @layerCount@ member of @srcSubresource@
--     or @dstSubresource@ /must/ not be
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS'
--
-- -   #VUID-VkImageBlit2-layerCount-08801# If one of the @layerCount@
--     members of @srcSubresource@ or @dstSubresource@ is
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the other
--     member /must/ be either
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' or equal to the
--     @arrayLayers@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
--     used to create the image minus @baseArrayLayer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageBlit2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_BLIT_2'
--
-- -   #VUID-VkImageBlit2-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--
-- -   #VUID-VkImageBlit2-sType-unique# The @sType@ value of each struct in
--     the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkImageBlit2-srcSubresource-parameter# @srcSubresource@ /must/
--     be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- -   #VUID-VkImageBlit2-dstSubresource-parameter# @dstSubresource@ /must/
--     be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'BlitImageInfo2',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageBlit2 (es :: [Type]) = ImageBlit2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @srcSubresource@ is the subresource to blit from.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffsets@ is a pointer to an array of two
    -- 'Vulkan.Core10.FundamentalTypes.Offset3D' structures specifying the
    -- bounds of the source region within @srcSubresource@.
    srcOffsets :: (Offset3D, Offset3D)
  , -- | @dstSubresource@ is the subresource to blit into.
    dstSubresource :: ImageSubresourceLayers
  , -- | @dstOffsets@ is a pointer to an array of two
    -- 'Vulkan.Core10.FundamentalTypes.Offset3D' structures specifying the
    -- bounds of the destination region within @dstSubresource@.
    dstOffsets :: (Offset3D, Offset3D)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageBlit2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageBlit2 es)

instance Extensible ImageBlit2 where
  extensibleTypeName = "ImageBlit2"
  setNext ImageBlit2{..} next' = ImageBlit2{next = next', ..}
  getNext ImageBlit2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageBlit2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CopyCommandTransformInfoQCOM = Just f
    | otherwise = Nothing

instance ( Extendss ImageBlit2 es
         , PokeChain es ) => ToCStruct (ImageBlit2 es) where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageBlit2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_BLIT_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 32 :: Ptr (FixedArray 2 Offset3D)))
    lift $ case (srcOffsets) of
      (e0, e1) -> do
        poke (pSrcOffsets' :: Ptr Offset3D) (e0)
        poke (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 72 :: Ptr (FixedArray 2 Offset3D)))
    lift $ case (dstOffsets) of
      (e0, e1) -> do
        poke (pDstOffsets' :: Ptr Offset3D) (e0)
        poke (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_BLIT_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 32 :: Ptr (FixedArray 2 Offset3D)))
    lift $ case ((zero, zero)) of
      (e0, e1) -> do
        poke (pSrcOffsets' :: Ptr Offset3D) (e0)
        poke (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageSubresourceLayers)) (zero)
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 72 :: Ptr (FixedArray 2 Offset3D)))
    lift $ case ((zero, zero)) of
      (e0, e1) -> do
        poke (pDstOffsets' :: Ptr Offset3D) (e0)
        poke (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    lift $ f

instance ( Extendss ImageBlit2 es
         , PeekChain es ) => FromCStruct (ImageBlit2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    let psrcOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 32 :: Ptr (FixedArray 2 Offset3D)))
    srcOffsets0 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    srcOffsets1 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 56 :: Ptr ImageSubresourceLayers))
    let pdstOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 72 :: Ptr (FixedArray 2 Offset3D)))
    dstOffsets0 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    dstOffsets1 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    pure $ ImageBlit2
             next
             srcSubresource
             ((srcOffsets0, srcOffsets1))
             dstSubresource
             ((dstOffsets0, dstOffsets1))

instance es ~ '[] => Zero (ImageBlit2 es) where
  zero = ImageBlit2
           ()
           zero
           (zero, zero)
           zero
           (zero, zero)


-- | VkBufferImageCopy2 - Structure specifying a buffer image copy operation
--
-- = Description
--
-- This structure is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy', but adds @sType@
-- and @pNext@ parameters, allowing it to be more easily extended.
--
-- == Valid Usage
--
-- -   #VUID-VkBufferImageCopy2-bufferRowLength-09101# @bufferRowLength@
--     /must/ be @0@, or greater than or equal to the @width@ member of
--     @imageExtent@
--
-- -   #VUID-VkBufferImageCopy2-bufferImageHeight-09102#
--     @bufferImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   #VUID-VkBufferImageCopy2-aspectMask-09103# The @aspectMask@ member
--     of @imageSubresource@ /must/ only have a single bit set
--
-- -   #VUID-VkBufferImageCopy2-imageExtent-06659# @imageExtent.width@
--     /must/ not be 0
--
-- -   #VUID-VkBufferImageCopy2-imageExtent-06660# @imageExtent.height@
--     /must/ not be 0
--
-- -   #VUID-VkBufferImageCopy2-imageExtent-06661# @imageExtent.depth@
--     /must/ not be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferImageCopy2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2'
--
-- -   #VUID-VkBufferImageCopy2-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--
-- -   #VUID-VkBufferImageCopy2-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBufferImageCopy2-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'CopyBufferToImageInfo2', 'CopyImageToBufferInfo2',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferImageCopy2 (es :: [Type]) = BufferImageCopy2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @bufferOffset@ is the offset in bytes from the start of the buffer
    -- object where the image data is copied from or to.
    bufferOffset :: DeviceSize
  , -- | @bufferRowLength@ and @bufferImageHeight@ specify in texels a subregion
    -- of a larger two- or three-dimensional image in buffer memory, and
    -- control the addressing calculations. If either of these values is zero,
    -- that aspect of the buffer memory is considered to be tightly packed
    -- according to the @imageExtent@.
    bufferRowLength :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy2" "bufferImageHeight"
    bufferImageHeight :: Word32
  , -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' used to
    -- specify the specific image subresources of the image used for the source
    -- or destination image data.
    imageSubresource :: ImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
    -- sub-region of the source or destination image data.
    imageOffset :: Offset3D
  , -- | @imageExtent@ is the size in texels of the image to copy in @width@,
    -- @height@ and @depth@.
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferImageCopy2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BufferImageCopy2 es)

instance Extensible BufferImageCopy2 where
  extensibleTypeName = "BufferImageCopy2"
  setNext BufferImageCopy2{..} next' = BufferImageCopy2{next = next', ..}
  getNext BufferImageCopy2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BufferImageCopy2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CopyCommandTransformInfoQCOM = Just f
    | otherwise = Nothing

instance ( Extendss BufferImageCopy2 es
         , PokeChain es ) => ToCStruct (BufferImageCopy2 es) where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferImageCopy2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (bufferOffset)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (bufferRowLength)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (bufferImageHeight)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    lift $ poke ((p `plusPtr` 48 :: Ptr Offset3D)) (imageOffset)
    lift $ poke ((p `plusPtr` 60 :: Ptr Extent3D)) (imageExtent)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr Offset3D)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Extent3D)) (zero)
    lift $ f

instance ( Extendss BufferImageCopy2 es
         , PeekChain es ) => FromCStruct (BufferImageCopy2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    bufferOffset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    bufferRowLength <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    bufferImageHeight <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 48 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 60 :: Ptr Extent3D))
    pure $ BufferImageCopy2
             next
             bufferOffset
             bufferRowLength
             bufferImageHeight
             imageSubresource
             imageOffset
             imageExtent

instance es ~ '[] => Zero (BufferImageCopy2 es) where
  zero = BufferImageCopy2
           ()
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkImageResolve2 - Structure specifying an image resolve operation
--
-- == Valid Usage
--
-- -   #VUID-VkImageResolve2-aspectMask-00266# The @aspectMask@ member of
--     @srcSubresource@ and @dstSubresource@ /must/ only contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageResolve2-layerCount-08803# If neither of the
--     @layerCount@ members of @srcSubresource@ or @dstSubresource@ are
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the
--     @layerCount@ member of @srcSubresource@ and @dstSubresource@ /must/
--     match
--
-- -   #VUID-VkImageResolve2-maintenance5-08802# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     feature is not enabled, the @layerCount@ member of @srcSubresource@
--     or @dstSubresource@ /must/ not be
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS'
--
-- -   #VUID-VkImageResolve2-layerCount-08804# If one of the @layerCount@
--     members of @srcSubresource@ or @dstSubresource@ is
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the other
--     member /must/ be either
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' or equal to the
--     @arrayLayers@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
--     used to create the image minus @baseArrayLayer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageResolve2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_RESOLVE_2'
--
-- -   #VUID-VkImageResolve2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkImageResolve2-srcSubresource-parameter# @srcSubresource@
--     /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- -   #VUID-VkImageResolve2-dstSubresource-parameter# @dstSubresource@
--     /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D', 'ResolveImageInfo2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageResolve2 = ImageResolve2
  { -- | @srcSubresource@ and @dstSubresource@ are
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' structures
    -- specifying the image subresources of the images used for the source and
    -- destination image data, respectively. Resolve of depth\/stencil images
    -- is not supported.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
    -- in texels of the sub-regions of the source and destination image data.
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageResolve2" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve2" "dstOffset"
    dstOffset :: Offset3D
  , -- | @extent@ is the size in texels of the source image to resolve in
    -- @width@, @height@ and @depth@.
    extent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageResolve2)
#endif
deriving instance Show ImageResolve2

instance ToCStruct ImageResolve2 where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageResolve2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_RESOLVE_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (srcOffset)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (dstOffset)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (extent)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_RESOLVE_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageResolve2 where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 60 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 72 :: Ptr Extent3D))
    pure $ ImageResolve2
             srcSubresource srcOffset dstSubresource dstOffset extent

instance Storable ImageResolve2 where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageResolve2 where
  zero = ImageResolve2
           zero
           zero
           zero
           zero
           zero


-- | VkCopyBufferInfo2 - Structure specifying parameters of a buffer copy
-- command
--
-- == Valid Usage
--
-- -   #VUID-VkCopyBufferInfo2-srcOffset-00113# The @srcOffset@ member of
--     each element of @pRegions@ /must/ be less than the size of
--     @srcBuffer@
--
-- -   #VUID-VkCopyBufferInfo2-dstOffset-00114# The @dstOffset@ member of
--     each element of @pRegions@ /must/ be less than the size of
--     @dstBuffer@
--
-- -   #VUID-VkCopyBufferInfo2-size-00115# The @size@ member of each
--     element of @pRegions@ /must/ be less than or equal to the size of
--     @srcBuffer@ minus @srcOffset@
--
-- -   #VUID-VkCopyBufferInfo2-size-00116# The @size@ member of each
--     element of @pRegions@ /must/ be less than or equal to the size of
--     @dstBuffer@ minus @dstOffset@
--
-- -   #VUID-VkCopyBufferInfo2-pRegions-00117# The union of the source
--     regions, and the union of the destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkCopyBufferInfo2-srcBuffer-00118# @srcBuffer@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkCopyBufferInfo2-srcBuffer-00119# If @srcBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyBufferInfo2-dstBuffer-00120# @dstBuffer@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkCopyBufferInfo2-dstBuffer-00121# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyBufferInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_BUFFER_INFO_2'
--
-- -   #VUID-VkCopyBufferInfo2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyBufferInfo2-srcBuffer-parameter# @srcBuffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkCopyBufferInfo2-dstBuffer-parameter# @dstBuffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkCopyBufferInfo2-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'BufferCopy2'
--     structures
--
-- -   #VUID-VkCopyBufferInfo2-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkCopyBufferInfo2-commonparent# Both of @dstBuffer@, and
--     @srcBuffer@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Buffer', 'BufferCopy2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdCopyBuffer2',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyBuffer2KHR'
data CopyBufferInfo2 = CopyBufferInfo2
  { -- | @srcBuffer@ is the source buffer.
    srcBuffer :: Buffer
  , -- | @dstBuffer@ is the destination buffer.
    dstBuffer :: Buffer
  , -- | @pRegions@ is a pointer to an array of 'BufferCopy2' structures
    -- specifying the regions to copy.
    regions :: Vector BufferCopy2
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyBufferInfo2)
#endif
deriving instance Show CopyBufferInfo2

instance ToCStruct CopyBufferInfo2 where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyBufferInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_INFO_2)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (srcBuffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr Buffer)) (dstBuffer)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @BufferCopy2 ((Data.Vector.length (regions)) * 40)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (40 * (i)) :: Ptr BufferCopy2) (e)) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr BufferCopy2))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    f

instance FromCStruct CopyBufferInfo2 where
  peekCStruct p = do
    srcBuffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    dstBuffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    regionCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pRegions <- peek @(Ptr BufferCopy2) ((p `plusPtr` 40 :: Ptr (Ptr BufferCopy2)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @BufferCopy2 ((pRegions `advancePtrBytes` (40 * (i)) :: Ptr BufferCopy2)))
    pure $ CopyBufferInfo2
             srcBuffer dstBuffer pRegions'

instance Zero CopyBufferInfo2 where
  zero = CopyBufferInfo2
           zero
           zero
           mempty


-- | VkCopyImageInfo2 - Structure specifying parameters of an image copy
-- command
--
-- == Valid Usage
--
-- -   #VUID-VkCopyImageInfo2-pRegions-00124# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01995# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkCopyImageInfo2-srcImageLayout-00128# @srcImageLayout@ /must/
--     specify the layout of the image subresources of @srcImage@ specified
--     in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyImageInfo2-srcImageLayout-01917# @srcImageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkCopyImageInfo2-dstImage-01996# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-VkCopyImageInfo2-dstImageLayout-00133# @dstImageLayout@ /must/
--     specify the layout of the image subresources of @dstImage@ specified
--     in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyImageInfo2-dstImageLayout-01395# @dstImageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01548# If the
--     'Vulkan.Core10.Enums.Format.Format' of each of @srcImage@ and
--     @dstImage@ is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     the 'Vulkan.Core10.Enums.Format.Format' of each of @srcImage@ and
--     @dstImage@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-size-compatibility size-compatible>
--
-- -   #VUID-VkCopyImageInfo2-None-01549# In a copy to or from a plane of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image>,
--     the 'Vulkan.Core10.Enums.Format.Format' of the image and plane
--     /must/ be compatible according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes the description of compatible planes>
--     for the plane being copied
--
-- -   #VUID-VkCopyImageInfo2-srcImage-09247# If the
--     'Vulkan.Core10.Enums.Format.Format' of each of @srcImage@ and
--     @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#compressed_image_formats compressed image format>,
--     the formats /must/ have the same texel block extent
--
-- -   #VUID-VkCopyImageInfo2-srcImage-00136# The sample count of
--     @srcImage@ and @dstImage@ /must/ match
--
-- -   #VUID-VkCopyImageInfo2-srcOffset-01783# The @srcOffset@ and @extent@
--     members of each element of @pRegions@ /must/ respect the image
--     transfer granularity requirements of @commandBuffer@’s command
--     pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-VkCopyImageInfo2-dstOffset-01784# The @dstOffset@ and @extent@
--     members of each element of @pRegions@ /must/ respect the image
--     transfer granularity requirements of @commandBuffer@’s command
--     pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01551# If neither @srcImage@ nor
--     @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     then for each element of @pRegions@, @srcSubresource.aspectMask@ and
--     @dstSubresource.aspectMask@ /must/ match
--
-- -   #VUID-VkCopyImageInfo2-srcImage-08713# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then for each element of @pRegions@, @srcSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageInfo2-dstImage-08714# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then for each element of @pRegions@, @dstSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01556# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     and the @dstImage@ does not have a multi-planar image format, then
--     for each element of @pRegions@, @dstSubresource.aspectMask@ /must/
--     be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkCopyImageInfo2-dstImage-01557# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     and the @srcImage@ does not have a multi-planar image format, then
--     for each element of @pRegions@, @srcSubresource.aspectMask@ /must/
--     be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkCopyImageInfo2-apiVersion-07932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--     extension is not enabled, or
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
--     is less than Vulkan 1.1, and either @srcImage@ or @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each
--     element of @pRegions@, @srcSubresource.baseArrayLayer@ and
--     @dstSubresource.baseArrayLayer@ /must/ both be @0@, and
--     @srcSubresource.layerCount@ and @dstSubresource.layerCount@ /must/
--     both be @1@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-04443# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @srcSubresource.baseArrayLayer@ /must/ be @0@ and
--     @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2-dstImage-04444# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and
--     @dstSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2-aspectMask-00142# For each element of
--     @pRegions@, @srcSubresource.aspectMask@ /must/ specify aspects
--     present in @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-aspectMask-00143# For each element of
--     @pRegions@, @dstSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-srcOffset-00144# For each element of
--     @pRegions@, @srcOffset.x@ and (@extent.width@ + @srcOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-srcOffset-00145# For each element of
--     @pRegions@, @srcOffset.y@ and (@extent.height@ + @srcOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-00146# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2-srcOffset-00147# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01785# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkCopyImageInfo2-dstImage-01786# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01787# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@
--
-- -   #VUID-VkCopyImageInfo2-dstImage-01788# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@
--
-- -   #VUID-VkCopyImageInfo2-apiVersion-07933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--     extension is not enabled, and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
--     is less than Vulkan 1.1, @srcImage@ and @dstImage@ /must/ have the
--     same 'Vulkan.Core10.Enums.ImageType.ImageType'
--
-- -   #VUID-VkCopyImageInfo2-apiVersion-08969# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--     extension is not enabled, and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
--     is less than Vulkan 1.1, @srcImage@ or @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @extent.depth@ /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-07743# If @srcImage@ and @dstImage@
--     have a different 'Vulkan.Core10.Enums.ImageType.ImageType', and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, one /must/ be
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D' and the other /must/
--     be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkCopyImageInfo2-srcImage-08793# If @srcImage@ and @dstImage@
--     have the same 'Vulkan.Core10.Enums.ImageType.ImageType', for each
--     element of @pRegions@, if neither of the @layerCount@ members of
--     @srcSubresource@ or @dstSubresource@ are
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the
--     @layerCount@ members of @srcSubresource@ or @dstSubresource@ /must/
--     match
--
-- -   #VUID-VkCopyImageInfo2-maintenance5-08792# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     feature is not enabled, the @layerCount@ member of @srcSubresource@
--     or @dstSubresource@ /must/ not be
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS'
--
-- -   #VUID-VkCopyImageInfo2-srcImage-08794# If @srcImage@ and @dstImage@
--     have the same 'Vulkan.Core10.Enums.ImageType.ImageType', and one of
--     the @layerCount@ members of @srcSubresource@ or @dstSubresource@ is
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the other
--     member /must/ be either
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' or equal to the
--     @arrayLayers@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
--     used to create the image minus @baseArrayLayer@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01790# If @srcImage@ and @dstImage@
--     are both of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     for each element of @pRegions@, @extent.depth@ /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01791# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each
--     element of @pRegions@, @extent.depth@ /must/ equal
--     @srcSubresource.layerCount@
--
-- -   #VUID-VkCopyImageInfo2-dstImage-01792# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each
--     element of @pRegions@, @extent.depth@ /must/ equal
--     @dstSubresource.layerCount@
--
-- -   #VUID-VkCopyImageInfo2-dstOffset-00150# For each element of
--     @pRegions@, @dstOffset.x@ and (@extent.width@ + @dstOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-dstOffset-00151# For each element of
--     @pRegions@, @dstOffset.y@ and (@extent.height@ + @dstOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-dstImage-00152# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2-dstOffset-00153# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-pRegions-07278# For each element of
--     @pRegions@, @srcOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-pRegions-07279# For each element of
--     @pRegions@, @srcOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-pRegions-07280# For each element of
--     @pRegions@, @srcOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-pRegions-07281# For each element of
--     @pRegions@, @dstOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-pRegions-07282# For each element of
--     @pRegions@, @dstOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-pRegions-07283# For each element of
--     @pRegions@, @dstOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01728# For each element of
--     @pRegions@, if the sum of @srcOffset.x@ and @extent.width@ does not
--     equal the width of the subresource specified by @srcSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01729# For each element of
--     @pRegions@, if the sum of @srcOffset.y@ and @extent.height@ does not
--     equal the height of the subresource specified by @srcSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-01730# For each element of
--     @pRegions@, if the sum of @srcOffset.z@ and @extent.depth@ does not
--     equal the depth of the subresource specified by @srcSubresource@,
--     @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-dstImage-01732# For each element of
--     @pRegions@, if the sum of @dstOffset.x@ and @extent.width@ does not
--     equal the width of the subresource specified by @dstSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-dstImage-01733# For each element of
--     @pRegions@, if the sum of @dstOffset.y@ and @extent.height@ does not
--     equal the height of the subresource specified by @dstSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-dstImage-01734# For each element of
--     @pRegions@, if the sum of @dstOffset.z@ and @extent.depth@ does not
--     equal the depth of the subresource specified by @dstSubresource@,
--     @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-aspect-06662# If the @aspect@ member of any
--     element of @pRegions@ includes any flag other than
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     or @srcImage@ was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     /must/ have been included in the
--     'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to create
--     @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-aspect-06663# If the @aspect@ member of any
--     element of @pRegions@ includes any flag other than
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     or @dstImage@ was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to create
--     @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-aspect-06664# If the @aspect@ member of any
--     element of @pRegions@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     and @srcImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     /must/ have been included in the
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     used to create @srcImage@
--
-- -   #VUID-VkCopyImageInfo2-aspect-06665# If the @aspect@ member of any
--     element of @pRegions@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     and @dstImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     used to create @dstImage@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-07966# If @srcImage@ is non-sparse
--     then the image or the specified /disjoint/ plane /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageInfo2-srcSubresource-07967# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkCopyImageInfo2-srcSubresource-07968# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ , if @srcSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, /must/ be less than or equal to the @arrayLayers@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@
--     was created
--
-- -   #VUID-VkCopyImageInfo2-srcImage-07969# @srcImage@ /must/ not have
--     been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageInfo2-dstImage-07966# If @dstImage@ is non-sparse
--     then the image or the specified /disjoint/ plane /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageInfo2-dstSubresource-07967# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyImageInfo2-dstSubresource-07968# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ , if @dstSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, /must/ be less than or equal to the @arrayLayers@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@
--     was created
--
-- -   #VUID-VkCopyImageInfo2-dstImage-07969# @dstImage@ /must/ not have
--     been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyImageInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_INFO_2'
--
-- -   #VUID-VkCopyImageInfo2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyImageInfo2-srcImage-parameter# @srcImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageInfo2-srcImageLayout-parameter# @srcImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkCopyImageInfo2-dstImage-parameter# @dstImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageInfo2-dstImageLayout-parameter# @dstImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkCopyImageInfo2-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'ImageCopy2'
--     structures
--
-- -   #VUID-VkCopyImageInfo2-regionCount-arraylength# @regionCount@ /must/
--     be greater than @0@
--
-- -   #VUID-VkCopyImageInfo2-commonparent# Both of @dstImage@, and
--     @srcImage@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Image', 'ImageCopy2',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdCopyImage2',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyImage2KHR'
data CopyImageInfo2 = CopyImageInfo2
  { -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the current layout of the source image subresource.
    srcImageLayout :: ImageLayout
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the current layout of the destination image
    -- subresource.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'ImageCopy2' structures
    -- specifying the regions to copy.
    regions :: Vector ImageCopy2
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageInfo2)
#endif
deriving instance Show CopyImageInfo2

instance ToCStruct CopyImageInfo2 where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_INFO_2)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @ImageCopy2 ((Data.Vector.length (regions)) * 88)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageCopy2) (e)) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr ImageCopy2))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyImageInfo2 where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageCopy2) ((p `plusPtr` 48 :: Ptr (Ptr ImageCopy2)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageCopy2 ((pRegions `advancePtrBytes` (88 * (i)) :: Ptr ImageCopy2)))
    pure $ CopyImageInfo2
             srcImage srcImageLayout dstImage dstImageLayout pRegions'

instance Zero CopyImageInfo2 where
  zero = CopyImageInfo2
           zero
           zero
           zero
           zero
           mempty


-- | VkBlitImageInfo2 - Structure specifying parameters of blit image command
--
-- == Valid Usage
--
-- -   #VUID-VkBlitImageInfo2-pRegions-00215# The source region specified
--     by each element of @pRegions@ /must/ be a region that is contained
--     within @srcImage@
--
-- -   #VUID-VkBlitImageInfo2-pRegions-00216# The destination region
--     specified by each element of @pRegions@ /must/ be a region that is
--     contained within @dstImage@
--
-- -   #VUID-VkBlitImageInfo2-pRegions-00217# The union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory with any texel that /may/ be sampled during the blit
--     operation
--
-- -   #VUID-VkBlitImageInfo2-srcImage-01999# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
--
-- -   #VUID-VkBlitImageInfo2-srcImage-06421# @srcImage@ /must/ not use a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion format that requires a sampler Y′CBCR conversion>
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00219# @srcImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00220# If @srcImage@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBlitImageInfo2-srcImageLayout-00221# @srcImageLayout@ /must/
--     specify the layout of the image subresources of @srcImage@ specified
--     in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkBlitImageInfo2-srcImageLayout-01398# @srcImageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkBlitImageInfo2-dstImage-02000# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_DST_BIT'
--
-- -   #VUID-VkBlitImageInfo2-dstImage-06422# @dstImage@ /must/ not use a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion format that requires a sampler Y′CBCR conversion>
--
-- -   #VUID-VkBlitImageInfo2-dstImage-00224# @dstImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkBlitImageInfo2-dstImage-00225# If @dstImage@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBlitImageInfo2-dstImageLayout-00226# @dstImageLayout@ /must/
--     specify the layout of the image subresources of @dstImage@ specified
--     in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkBlitImageInfo2-dstImageLayout-01399# @dstImageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00229# If either of @srcImage@ or
--     @dstImage@ was created with a signed integer
--     'Vulkan.Core10.Enums.Format.Format', the other /must/ also have been
--     created with a signed integer 'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00230# If either of @srcImage@ or
--     @dstImage@ was created with an unsigned integer
--     'Vulkan.Core10.Enums.Format.Format', the other /must/ also have been
--     created with an unsigned integer 'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00231# If either of @srcImage@ or
--     @dstImage@ was created with a depth\/stencil format, the other
--     /must/ have exactly the same format
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00232# If @srcImage@ was created
--     with a depth\/stencil format, @filter@ /must/ be
--     'Vulkan.Core10.Enums.Filter.FILTER_NEAREST'
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00233# @srcImage@ /must/ have been
--     created with a @samples@ value of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkBlitImageInfo2-dstImage-00234# @dstImage@ /must/ have been
--     created with a @samples@ value of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkBlitImageInfo2-filter-02001# If @filter@ is
--     'Vulkan.Core10.Enums.Filter.FILTER_LINEAR', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-VkBlitImageInfo2-filter-02002# If @filter@ is
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-VkBlitImageInfo2-filter-00237# If @filter@ is
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT', @srcImage@ /must/ be
--     of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkBlitImageInfo2-srcSubresource-01705# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkBlitImageInfo2-dstSubresource-01706# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkBlitImageInfo2-srcSubresource-01707# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ , if @srcSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, /must/ be less than or equal to the @arrayLayers@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@
--     was created
--
-- -   #VUID-VkBlitImageInfo2-dstSubresource-01708# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ , if @srcSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, /must/ be less than or equal to the @arrayLayers@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@
--     was created
--
-- -   #VUID-VkBlitImageInfo2-dstImage-02545# @dstImage@ and @srcImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00240# If either @srcImage@ or
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     then for each element of @pRegions@, @srcSubresource.baseArrayLayer@
--     and @dstSubresource.baseArrayLayer@ /must/ each be @0@, and
--     @srcSubresource.layerCount@ and @dstSubresource.layerCount@ /must/
--     each be @1@
--
-- -   #VUID-VkBlitImageInfo2-aspectMask-00241# For each element of
--     @pRegions@, @srcSubresource.aspectMask@ /must/ specify aspects
--     present in @srcImage@
--
-- -   #VUID-VkBlitImageInfo2-aspectMask-00242# For each element of
--     @pRegions@, @dstSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-VkBlitImageInfo2-srcOffset-00243# For each element of
--     @pRegions@, @srcOffsets@[0].x and @srcOffsets@[1].x /must/ both be
--     greater than or equal to @0@ and less than or equal to the width of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkBlitImageInfo2-srcOffset-00244# For each element of
--     @pRegions@, @srcOffsets@[0].y and @srcOffsets@[1].y /must/ both be
--     greater than or equal to @0@ and less than or equal to the height of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00245# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffsets@[0].y /must/ be @0@ and @srcOffsets@[1].y
--     /must/ be @1@
--
-- -   #VUID-VkBlitImageInfo2-srcOffset-00246# For each element of
--     @pRegions@, @srcOffsets@[0].z and @srcOffsets@[1].z /must/ both be
--     greater than or equal to @0@ and less than or equal to the depth of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkBlitImageInfo2-srcImage-00247# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffsets@[0].z /must/ be @0@ and @srcOffsets@[1].z
--     /must/ be @1@
--
-- -   #VUID-VkBlitImageInfo2-dstOffset-00248# For each element of
--     @pRegions@, @dstOffsets@[0].x and @dstOffsets@[1].x /must/ both be
--     greater than or equal to @0@ and less than or equal to the width of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkBlitImageInfo2-dstOffset-00249# For each element of
--     @pRegions@, @dstOffsets@[0].y and @dstOffsets@[1].y /must/ both be
--     greater than or equal to @0@ and less than or equal to the height of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkBlitImageInfo2-dstImage-00250# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffsets@[0].y /must/ be @0@ and @dstOffsets@[1].y
--     /must/ be @1@
--
-- -   #VUID-VkBlitImageInfo2-dstOffset-00251# For each element of
--     @pRegions@, @dstOffsets@[0].z and @dstOffsets@[1].z /must/ both be
--     greater than or equal to @0@ and less than or equal to the depth of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkBlitImageInfo2-dstImage-00252# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffsets@[0].z /must/ be @0@ and @dstOffsets@[1].z
--     /must/ be @1@
--
-- -   #VUID-VkBlitImageInfo2-pRegions-04561# If any element of @pRegions@
--     contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @srcImage@ and @dstImage@ /must/ not be
--     block-compressed images
--
-- -   #VUID-VkBlitImageInfo2KHR-pRegions-06207# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @srcImage@ /must/ be of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkBlitImageInfo2KHR-pRegions-06208# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @srcImage@ /must/ not have a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-VkBlitImageInfo2-filter-09204# If @filter@ is
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' and if the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-filter-cubic-weight-selection selectableCubicWeights>
--     feature is not enabled then the cubic weights /must/ be
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBlitImageInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BLIT_IMAGE_INFO_2'
--
-- -   #VUID-VkBlitImageInfo2-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.BlitImageCubicWeightsInfoQCOM'
--
-- -   #VUID-VkBlitImageInfo2-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBlitImageInfo2-srcImage-parameter# @srcImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkBlitImageInfo2-srcImageLayout-parameter# @srcImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkBlitImageInfo2-dstImage-parameter# @dstImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkBlitImageInfo2-dstImageLayout-parameter# @dstImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkBlitImageInfo2-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'ImageBlit2'
--     structures
--
-- -   #VUID-VkBlitImageInfo2-filter-parameter# @filter@ /must/ be a valid
--     'Vulkan.Core10.Enums.Filter.Filter' value
--
-- -   #VUID-VkBlitImageInfo2-regionCount-arraylength# @regionCount@ /must/
--     be greater than @0@
--
-- -   #VUID-VkBlitImageInfo2-commonparent# Both of @dstImage@, and
--     @srcImage@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.Filter.Filter', 'Vulkan.Core10.Handles.Image',
-- 'ImageBlit2', 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdBlitImage2',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdBlitImage2KHR'
data BlitImageInfo2 (es :: [Type]) = BlitImageInfo2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- blit.
    srcImageLayout :: ImageLayout
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the blit.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'ImageBlit2' structures
    -- specifying the regions to blit.
    regions :: Vector (SomeStruct ImageBlit2)
  , -- | @filter@ is a 'Vulkan.Core10.Enums.Filter.Filter' specifying the filter
    -- to apply if the blits require scaling.
    filter' :: Filter
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BlitImageInfo2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BlitImageInfo2 es)

instance Extensible BlitImageInfo2 where
  extensibleTypeName = "BlitImageInfo2"
  setNext BlitImageInfo2{..} next' = BlitImageInfo2{next = next', ..}
  getNext BlitImageInfo2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BlitImageInfo2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BlitImageCubicWeightsInfoQCOM = Just f
    | otherwise = Nothing

instance ( Extendss BlitImageInfo2 es
         , PokeChain es ) => ToCStruct (BlitImageInfo2 es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BlitImageInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BLIT_IMAGE_INFO_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @(ImageBlit2 _) ((Data.Vector.length (regions)) * 96)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (96 * (i)) :: Ptr (ImageBlit2 _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (ImageBlit2 _)))) (pPRegions')
    lift $ poke ((p `plusPtr` 56 :: Ptr Filter)) (filter')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BLIT_IMAGE_INFO_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Filter)) (zero)
    lift $ f

instance ( Extendss BlitImageInfo2 es
         , PeekChain es ) => FromCStruct (BlitImageInfo2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr (ImageBlit2 _)) ((p `plusPtr` 48 :: Ptr (Ptr (ImageBlit2 _))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (96 * (i)) :: Ptr (ImageBlit2 _)))))
    filter' <- peek @Filter ((p `plusPtr` 56 :: Ptr Filter))
    pure $ BlitImageInfo2
             next
             srcImage
             srcImageLayout
             dstImage
             dstImageLayout
             pRegions'
             filter'

instance es ~ '[] => Zero (BlitImageInfo2 es) where
  zero = BlitImageInfo2
           ()
           zero
           zero
           zero
           zero
           mempty
           zero


-- | VkCopyBufferToImageInfo2 - Structure specifying parameters of a buffer
-- to image copy command
--
-- == Valid Usage
--
-- -   #VUID-VkCopyBufferToImageInfo2-pRegions-04565# The image region
--     specified by each element of @pRegions@ that does not contain
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain /must/ be contained within the specified
--     @imageSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-04554# If the image
--     region specified by each element of @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, the rotated destination region as described in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies-buffers-images-rotation-addressing>
--     /must/ be contained within @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-04555# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @dstImage@ /must/ have a 1x1x1
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility-classes texel block extent>
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-06203# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @dstImage@ /must/ be of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-06204# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @dstImage@ /must/ not have a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-VkCopyBufferToImageInfo2-pRegions-00171# @srcBuffer@ /must/ be
--     large enough to contain all buffer locations that are accessed
--     according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-VkCopyBufferToImageInfo2-pRegions-00173# The union of all
--     source regions, and the union of all destination regions, specified
--     by the elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkCopyBufferToImageInfo2-srcBuffer-00174# @srcBuffer@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-01997# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-VkCopyBufferToImageInfo2-srcBuffer-00176# If @srcBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-00177# @dstImage@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImageLayout-00180#
--     @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImageLayout-01396#
--     @dstImageLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkCopyBufferToImageInfo2-pRegions-07931# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_range_unrestricted VK_EXT_depth_range_unrestricted>
--     is not enabled, for each element of @pRegions@ whose
--     @imageSubresource@ contains a depth aspect, the data in @srcBuffer@
--     /must/ be in the range [0,1]
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07966# If @dstImage@ is
--     non-sparse then the image or the specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyBufferToImageInfo2-imageSubresource-07967# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyBufferToImageInfo2-imageSubresource-07968# The
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ , if @imageSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, /must/ be less than or equal to the @arrayLayers@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@
--     was created
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07969# @dstImage@ /must/ not
--     have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07973# @dstImage@ /must/
--     have a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07979# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyBufferToImageInfo2-imageOffset-09104# For each element
--     of @pRegions@, @imageOffset.z@ and (@imageExtent.depth@ +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07980# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07274# For each element of
--     @pRegions@, @imageOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07275# For each element of
--     @pRegions@, @imageOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07276# For each element of
--     @pRegions@, @imageOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-00207# For each element of
--     @pRegions@, if the sum of @imageOffset.x@ and @extent.width@ does
--     not equal the width of the subresource specified by
--     @srcSubresource@, @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-00208# For each element of
--     @pRegions@, if the sum of @imageOffset.y@ and @extent.height@ does
--     not equal the height of the subresource specified by
--     @srcSubresource@, @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-00209# For each element of
--     @pRegions@, if the sum of @imageOffset.z@ and @extent.depth@ does
--     not equal the depth of the subresource specified by
--     @srcSubresource@, @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-imageSubresource-09105# For each
--     element of @pRegions@, @imageSubresource.aspectMask@ /must/ specify
--     aspects present in @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07981# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07983# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be @0@ and
--     @imageSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyBufferToImageInfo2-bufferRowLength-09106# For each
--     element of @pRegions@, @bufferRowLength@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-bufferImageHeight-09107# For each
--     element of @pRegions@, @bufferImageHeight@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-bufferRowLength-09108# For each
--     element of @pRegions@, @bufferRowLength@ divided by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     and then multiplied by the texel block size of @dstImage@ /must/ be
--     less than or equal to 231-1
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07975# If @dstImage@ does
--     not have either a depth\/stencil format or a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block size>
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07976# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the element size of the compatible format for the format
--     and the @aspectMask@ of the @imageSubresource@ as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes ???>
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-07978# If @dstImage@ has a
--     depth\/stencil format, the @bufferOffset@ member of any element of
--     @pRegions@ /must/ be a multiple of @4@
--
-- -   #VUID-VkCopyBufferToImageInfo2-pRegions-06223# For each element of
--     @pRegions@ not containing
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2-pRegions-06224# For each element of
--     @pRegions@ not containing
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, @imageOffset.y@ and (@imageExtent.height@ +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @dstImage@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyBufferToImageInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2'
--
-- -   #VUID-VkCopyBufferToImageInfo2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyBufferToImageInfo2-srcBuffer-parameter# @srcBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImage-parameter# @dstImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyBufferToImageInfo2-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyBufferToImageInfo2-pRegions-parameter# @pRegions@ /must/
--     be a valid pointer to an array of @regionCount@ valid
--     'BufferImageCopy2' structures
--
-- -   #VUID-VkCopyBufferToImageInfo2-regionCount-arraylength#
--     @regionCount@ /must/ be greater than @0@
--
-- -   #VUID-VkCopyBufferToImageInfo2-commonparent# Both of @dstImage@, and
--     @srcBuffer@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Buffer', 'BufferImageCopy2',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyBufferToImage2',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyBufferToImage2KHR'
data CopyBufferToImageInfo2 = CopyBufferToImageInfo2
  { -- | @srcBuffer@ is the source buffer.
    srcBuffer :: Buffer
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the copy.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'BufferImageCopy2' structures
    -- specifying the regions to copy.
    regions :: Vector (SomeStruct BufferImageCopy2)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyBufferToImageInfo2)
#endif
deriving instance Show CopyBufferToImageInfo2

instance ToCStruct CopyBufferToImageInfo2 where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyBufferToImageInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (srcBuffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @(BufferImageCopy2 _) ((Data.Vector.length (regions)) * 72)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (72 * (i)) :: Ptr (BufferImageCopy2 _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (BufferImageCopy2 _)))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyBufferToImageInfo2 where
  peekCStruct p = do
    srcBuffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    dstImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pRegions <- peek @(Ptr (BufferImageCopy2 _)) ((p `plusPtr` 40 :: Ptr (Ptr (BufferImageCopy2 _))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr (BufferImageCopy2 _)))))
    pure $ CopyBufferToImageInfo2
             srcBuffer dstImage dstImageLayout pRegions'

instance Zero CopyBufferToImageInfo2 where
  zero = CopyBufferToImageInfo2
           zero
           zero
           zero
           mempty


-- | VkCopyImageToBufferInfo2 - Structure specifying parameters of an image
-- to buffer copy command
--
-- == Valid Usage
--
-- -   #VUID-VkCopyImageToBufferInfo2-pRegions-04566# The image region
--     specified by each element of @pRegions@ that does not contain
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain /must/ be contained within the specified
--     @imageSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-04557# If the image
--     region specified by each element of @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, the rotated source region as described in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies-buffers-images-rotation-addressing>
--     /must/ be contained within @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-04558# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @srcImage@ /must/ have a 1x1x1
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility-classes texel block extent>
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-06205# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @srcImage@ /must/ be of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-06206# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @srcImage@ /must/ not have a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-VkCopyImageToBufferInfo2-pRegions-00183# @dstBuffer@ /must/ be
--     large enough to contain all buffer locations that are accessed
--     according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-VkCopyImageToBufferInfo2-pRegions-00184# The union of all
--     source regions, and the union of all destination regions, specified
--     by the elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-00186# @srcImage@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-01998# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkCopyImageToBufferInfo2-dstBuffer-00191# @dstBuffer@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkCopyImageToBufferInfo2-dstBuffer-00192# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImageLayout-00189#
--     @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImageLayout-01397#
--     @srcImageLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07966# If @srcImage@ is
--     non-sparse then the image or the specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToBufferInfo2-imageSubresource-07967# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToBufferInfo2-imageSubresource-07968# The
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ , if @imageSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, /must/ be less than or equal to the @arrayLayers@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@
--     was created
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07969# @srcImage@ /must/ not
--     have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07973# @srcImage@ /must/
--     have a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07979# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToBufferInfo2-imageOffset-09104# For each element
--     of @pRegions@, @imageOffset.z@ and (@imageExtent.depth@ +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07980# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07274# For each element of
--     @pRegions@, @imageOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07275# For each element of
--     @pRegions@, @imageOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07276# For each element of
--     @pRegions@, @imageOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-00207# For each element of
--     @pRegions@, if the sum of @imageOffset.x@ and @extent.width@ does
--     not equal the width of the subresource specified by
--     @srcSubresource@, @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-00208# For each element of
--     @pRegions@, if the sum of @imageOffset.y@ and @extent.height@ does
--     not equal the height of the subresource specified by
--     @srcSubresource@, @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-00209# For each element of
--     @pRegions@, if the sum of @imageOffset.z@ and @extent.depth@ does
--     not equal the depth of the subresource specified by
--     @srcSubresource@, @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-imageSubresource-09105# For each
--     element of @pRegions@, @imageSubresource.aspectMask@ /must/ specify
--     aspects present in @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07981# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07983# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be @0@ and
--     @imageSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToBufferInfo2-bufferRowLength-09106# For each
--     element of @pRegions@, @bufferRowLength@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-bufferImageHeight-09107# For each
--     element of @pRegions@, @bufferImageHeight@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-bufferRowLength-09108# For each
--     element of @pRegions@, @bufferRowLength@ divided by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     and then multiplied by the texel block size of @srcImage@ /must/ be
--     less than or equal to 231-1
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07975# If @srcImage@ does
--     not have either a depth\/stencil format or a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block size>
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07976# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the element size of the compatible format for the format
--     and the @aspectMask@ of the @imageSubresource@ as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes ???>
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-07978# If @srcImage@ has a
--     depth\/stencil format, the @bufferOffset@ member of any element of
--     @pRegions@ /must/ be a multiple of @4@
--
-- -   #VUID-VkCopyImageToBufferInfo2-imageOffset-00197# For each element
--     of @pRegions@ not containing
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-VkCopyImageToBufferInfo2-imageOffset-00198# For each element
--     of @pRegions@ not containing
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, @imageOffset.y@ and (@imageExtent.height@ +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @srcImage@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyImageToBufferInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2'
--
-- -   #VUID-VkCopyImageToBufferInfo2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImage-parameter# @srcImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageToBufferInfo2-srcImageLayout-parameter#
--     @srcImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyImageToBufferInfo2-dstBuffer-parameter# @dstBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkCopyImageToBufferInfo2-pRegions-parameter# @pRegions@ /must/
--     be a valid pointer to an array of @regionCount@ valid
--     'BufferImageCopy2' structures
--
-- -   #VUID-VkCopyImageToBufferInfo2-regionCount-arraylength#
--     @regionCount@ /must/ be greater than @0@
--
-- -   #VUID-VkCopyImageToBufferInfo2-commonparent# Both of @dstBuffer@,
--     and @srcImage@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Buffer', 'BufferImageCopy2',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyImageToBuffer2',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyImageToBuffer2KHR'
data CopyImageToBufferInfo2 = CopyImageToBufferInfo2
  { -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- copy.
    srcImageLayout :: ImageLayout
  , -- | @dstBuffer@ is the destination buffer.
    dstBuffer :: Buffer
  , -- | @pRegions@ is a pointer to an array of 'BufferImageCopy2' structures
    -- specifying the regions to copy.
    regions :: Vector (SomeStruct BufferImageCopy2)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageToBufferInfo2)
#endif
deriving instance Show CopyImageToBufferInfo2

instance ToCStruct CopyImageToBufferInfo2 where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageToBufferInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Buffer)) (dstBuffer)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @(BufferImageCopy2 _) ((Data.Vector.length (regions)) * 72)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (72 * (i)) :: Ptr (BufferImageCopy2 _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (BufferImageCopy2 _)))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Buffer)) (zero)
    f

instance FromCStruct CopyImageToBufferInfo2 where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstBuffer <- peek @Buffer ((p `plusPtr` 32 :: Ptr Buffer))
    regionCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pRegions <- peek @(Ptr (BufferImageCopy2 _)) ((p `plusPtr` 48 :: Ptr (Ptr (BufferImageCopy2 _))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr (BufferImageCopy2 _)))))
    pure $ CopyImageToBufferInfo2
             srcImage srcImageLayout dstBuffer pRegions'

instance Zero CopyImageToBufferInfo2 where
  zero = CopyImageToBufferInfo2
           zero
           zero
           zero
           mempty


-- | VkResolveImageInfo2 - Structure specifying parameters of resolve image
-- command
--
-- == Valid Usage
--
-- -   #VUID-VkResolveImageInfo2-pRegions-00255# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkResolveImageInfo2-srcImage-00256# If @srcImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkResolveImageInfo2-srcImage-00257# @srcImage@ /must/ have a
--     sample count equal to any valid sample count value other than
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkResolveImageInfo2-dstImage-00258# If @dstImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkResolveImageInfo2-dstImage-00259# @dstImage@ /must/ have a
--     sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkResolveImageInfo2-srcImageLayout-00260# @srcImageLayout@
--     /must/ specify the layout of the image subresources of @srcImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkResolveImageInfo2-srcImageLayout-01400# @srcImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkResolveImageInfo2-dstImageLayout-00262# @dstImageLayout@
--     /must/ specify the layout of the image subresources of @dstImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkResolveImageInfo2-dstImageLayout-01401# @dstImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkResolveImageInfo2-dstImage-02003# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkResolveImageInfo2-linearColorAttachment-06519# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-linearColorAttachment linearColorAttachment>
--     feature is enabled and the image is created with
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
--
-- -   #VUID-VkResolveImageInfo2-srcImage-01386# @srcImage@ and @dstImage@
--     /must/ have been created with the same image format
--
-- -   #VUID-VkResolveImageInfo2-srcSubresource-01709# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkResolveImageInfo2-dstSubresource-01710# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkResolveImageInfo2-srcSubresource-01711# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ , if @srcSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, /must/ be less than or equal to the @arrayLayers@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@
--     was created
--
-- -   #VUID-VkResolveImageInfo2-dstSubresource-01712# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ , if @dstSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     is not enabled, /must/ be less than or equal to the @arrayLayers@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@
--     was created
--
-- -   #VUID-VkResolveImageInfo2-dstImage-02546# @dstImage@ and @srcImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkResolveImageInfo2-srcImage-04446# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkResolveImageInfo2-srcImage-04447# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and
--     @dstSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkResolveImageInfo2-srcOffset-00269# For each element of
--     @pRegions@, @srcOffset.x@ and (@extent.width@ + @srcOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkResolveImageInfo2-srcOffset-00270# For each element of
--     @pRegions@, @srcOffset.y@ and (@extent.height@ + @srcOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkResolveImageInfo2-srcImage-00271# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-VkResolveImageInfo2-srcOffset-00272# For each element of
--     @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkResolveImageInfo2-srcImage-00273# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkResolveImageInfo2-dstOffset-00274# For each element of
--     @pRegions@, @dstOffset.x@ and (@extent.width@ + @dstOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkResolveImageInfo2-dstOffset-00275# For each element of
--     @pRegions@, @dstOffset.y@ and (@extent.height@ + @dstOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkResolveImageInfo2-dstImage-00276# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-VkResolveImageInfo2-dstOffset-00277# For each element of
--     @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkResolveImageInfo2-dstImage-00278# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkResolveImageInfo2-srcImage-06762# @srcImage@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkResolveImageInfo2-srcImage-06763# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkResolveImageInfo2-dstImage-06764# @dstImage@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkResolveImageInfo2-dstImage-06765# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkResolveImageInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2'
--
-- -   #VUID-VkResolveImageInfo2-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkResolveImageInfo2-srcImage-parameter# @srcImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkResolveImageInfo2-srcImageLayout-parameter# @srcImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkResolveImageInfo2-dstImage-parameter# @dstImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkResolveImageInfo2-dstImageLayout-parameter# @dstImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkResolveImageInfo2-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'ImageResolve2'
--     structures
--
-- -   #VUID-VkResolveImageInfo2-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkResolveImageInfo2-commonparent# Both of @dstImage@, and
--     @srcImage@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'ImageResolve2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdResolveImage2',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdResolveImage2KHR'
data ResolveImageInfo2 = ResolveImageInfo2
  { -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- resolve.
    srcImageLayout :: ImageLayout
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the resolve.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'ImageResolve2' structures
    -- specifying the regions to resolve.
    regions :: Vector ImageResolve2
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ResolveImageInfo2)
#endif
deriving instance Show ResolveImageInfo2

instance ToCStruct ResolveImageInfo2 where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ResolveImageInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @ImageResolve2 ((Data.Vector.length (regions)) * 88)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageResolve2) (e)) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr ImageResolve2))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct ResolveImageInfo2 where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageResolve2) ((p `plusPtr` 48 :: Ptr (Ptr ImageResolve2)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageResolve2 ((pRegions `advancePtrBytes` (88 * (i)) :: Ptr ImageResolve2)))
    pure $ ResolveImageInfo2
             srcImage srcImageLayout dstImage dstImageLayout pRegions'

instance Zero ResolveImageInfo2 where
  zero = ResolveImageInfo2
           zero
           zero
           zero
           zero
           mempty

