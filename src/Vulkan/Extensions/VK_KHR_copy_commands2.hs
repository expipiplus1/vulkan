{-# language CPP #-}
-- | = Name
--
-- VK_KHR_copy_commands2 - device extension
--
-- == VK_KHR_copy_commands2
--
-- [__Name String__]
--     @VK_KHR_copy_commands2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     338
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_copy_commands2:%20&body=@jackohound%20 >
--
-- == Other Extension Metadata
--
-- [Last Modified Date]
--     2020-07-06
--
-- [__Interactions and External Dependencies__]
--
--     -   None
--
-- [Contributors]
--
--     -   Jeff Leger, Qualcomm
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Tom Olson, ARM
--
-- == Description
--
-- This extension provides extensible versions of the Vulkan buffer and
-- image copy commands. The new commands are functionally identical to the
-- core commands, except that their copy parameters are specified using
-- extensible structures that can be used to pass extension-specific
-- information.
--
-- The following extensible copy commands are introduced with this
-- extension: 'cmdCopyBuffer2KHR', 'cmdCopyImage2KHR',
-- 'cmdCopyBufferToImage2KHR', 'cmdCopyImageToBuffer2KHR',
-- 'cmdBlitImage2KHR', and 'cmdResolveImage2KHR'. Each command
-- contain@*Info2KHR@ structure parameter that includes @sType@\/@pNext@
-- members. Lower level structures that describe each region to be copied
-- are also extended with @sType@\/@pNext@ members.
--
-- == New Commands
--
-- -   'cmdBlitImage2KHR'
--
-- -   'cmdCopyBuffer2KHR'
--
-- -   'cmdCopyBufferToImage2KHR'
--
-- -   'cmdCopyImage2KHR'
--
-- -   'cmdCopyImageToBuffer2KHR'
--
-- -   'cmdResolveImage2KHR'
--
-- == New Structures
--
-- -   'BlitImageInfo2KHR'
--
-- -   'BufferCopy2KHR'
--
-- -   'BufferImageCopy2KHR'
--
-- -   'CopyBufferInfo2KHR'
--
-- -   'CopyBufferToImageInfo2KHR'
--
-- -   'CopyImageInfo2KHR'
--
-- -   'CopyImageToBufferInfo2KHR'
--
-- -   'ImageBlit2KHR'
--
-- -   'ImageCopy2KHR'
--
-- -   'ImageResolve2KHR'
--
-- -   'ResolveImageInfo2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_COPY_COMMANDS_2_EXTENSION_NAME'
--
-- -   'KHR_COPY_COMMANDS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COPY_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_BLIT_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COPY_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-07-06 (Jeff Leger)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'BlitImageInfo2KHR', 'BufferCopy2KHR', 'BufferImageCopy2KHR',
-- 'CopyBufferInfo2KHR', 'CopyBufferToImageInfo2KHR', 'CopyImageInfo2KHR',
-- 'CopyImageToBufferInfo2KHR', 'ImageBlit2KHR', 'ImageCopy2KHR',
-- 'ImageResolve2KHR', 'ResolveImageInfo2KHR', 'cmdBlitImage2KHR',
-- 'cmdCopyBuffer2KHR', 'cmdCopyBufferToImage2KHR', 'cmdCopyImage2KHR',
-- 'cmdCopyImageToBuffer2KHR', 'cmdResolveImage2KHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_copy_commands2  ( cmdCopyBuffer2KHR
                                                , cmdCopyImage2KHR
                                                , cmdBlitImage2KHR
                                                , cmdCopyBufferToImage2KHR
                                                , cmdCopyImageToBuffer2KHR
                                                , cmdResolveImage2KHR
                                                , BufferCopy2KHR(..)
                                                , ImageCopy2KHR(..)
                                                , ImageBlit2KHR(..)
                                                , BufferImageCopy2KHR(..)
                                                , ImageResolve2KHR(..)
                                                , CopyBufferInfo2KHR(..)
                                                , CopyImageInfo2KHR(..)
                                                , BlitImageInfo2KHR(..)
                                                , CopyBufferToImageInfo2KHR(..)
                                                , CopyImageToBufferInfo2KHR(..)
                                                , ResolveImageInfo2KHR(..)
                                                , KHR_COPY_COMMANDS_2_SPEC_VERSION
                                                , pattern KHR_COPY_COMMANDS_2_SPEC_VERSION
                                                , KHR_COPY_COMMANDS_2_EXTENSION_NAME
                                                , pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME
                                                ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_rotated_copy_commands (CopyCommandTransformInfoQCOM)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBlitImage2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBuffer2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBufferToImage2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImage2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImageToBuffer2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResolveImage2KHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COPY_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_BLIT_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_COPY_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBuffer2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyBufferInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyBufferInfo2KHR -> IO ()

-- | vkCmdCopyBuffer2KHR - Copy data between buffer regions
--
-- = Description
--
-- This command is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBuffer', but includes
-- extensible sub-structures that include @sType@ and @pNext@ parameters,
-- allowing them to be more easily extended.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyBuffer2KHR-commandBuffer-01822# If @commandBuffer@ is
--     an unprotected command buffer, then @srcBuffer@ /must/ not be a
--     protected buffer
--
-- -   #VUID-vkCmdCopyBuffer2KHR-commandBuffer-01823# If @commandBuffer@ is
--     an unprotected command buffer, then @dstBuffer@ /must/ not be a
--     protected buffer
--
-- -   #VUID-vkCmdCopyBuffer2KHR-commandBuffer-01824# If @commandBuffer@ is
--     a protected command buffer, then @dstBuffer@ /must/ not be an
--     unprotected buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyBuffer2KHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyBuffer2KHR-pCopyBufferInfo-parameter#
--     @pCopyBufferInfo@ /must/ be a valid pointer to a valid
--     'CopyBufferInfo2KHR' structure
--
-- -   #VUID-vkCmdCopyBuffer2KHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyBuffer2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyBuffer2KHR-renderpass# This command /must/ only be
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyBufferInfo2KHR'
cmdCopyBuffer2KHR :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command will be
                     -- recorded.
                     CommandBuffer
                  -> -- | @pCopyBufferInfo@ is a pointer to a 'CopyBufferInfo2KHR' structure
                     -- describing the copy parameters.
                     CopyBufferInfo2KHR
                  -> io ()
cmdCopyBuffer2KHR commandBuffer copyBufferInfo = liftIO . evalContT $ do
  let vkCmdCopyBuffer2KHRPtr = pVkCmdCopyBuffer2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyBuffer2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBuffer2KHR is null" Nothing Nothing
  let vkCmdCopyBuffer2KHR' = mkVkCmdCopyBuffer2KHR vkCmdCopyBuffer2KHRPtr
  pCopyBufferInfo <- ContT $ withCStruct (copyBufferInfo)
  lift $ traceAroundEvent "vkCmdCopyBuffer2KHR" (vkCmdCopyBuffer2KHR' (commandBufferHandle (commandBuffer)) pCopyBufferInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImage2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyImageInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyImageInfo2KHR -> IO ()

-- | vkCmdCopyImage2KHR - Copy data between images
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
-- -   #VUID-vkCmdCopyImage2KHR-commandBuffer-01825# If @commandBuffer@ is
--     an unprotected command buffer, then @srcImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdCopyImage2KHR-commandBuffer-01826# If @commandBuffer@ is
--     an unprotected command buffer, then @dstImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdCopyImage2KHR-commandBuffer-01827# If @commandBuffer@ is
--     a protected command buffer, then @dstImage@ /must/ not be an
--     unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyImage2KHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyImage2KHR-pCopyImageInfo-parameter# @pCopyImageInfo@
--     /must/ be a valid pointer to a valid 'CopyImageInfo2KHR' structure
--
-- -   #VUID-vkCmdCopyImage2KHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyImage2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyImage2KHR-renderpass# This command /must/ only be
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyImageInfo2KHR'
cmdCopyImage2KHR :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is the command buffer into which the command will be
                    -- recorded.
                    CommandBuffer
                 -> -- | @pCopyImageInfo@ is a pointer to a 'CopyImageInfo2KHR' structure
                    -- describing the copy parameters.
                    CopyImageInfo2KHR
                 -> io ()
cmdCopyImage2KHR commandBuffer copyImageInfo = liftIO . evalContT $ do
  let vkCmdCopyImage2KHRPtr = pVkCmdCopyImage2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImage2KHR is null" Nothing Nothing
  let vkCmdCopyImage2KHR' = mkVkCmdCopyImage2KHR vkCmdCopyImage2KHRPtr
  pCopyImageInfo <- ContT $ withCStruct (copyImageInfo)
  lift $ traceAroundEvent "vkCmdCopyImage2KHR" (vkCmdCopyImage2KHR' (commandBufferHandle (commandBuffer)) pCopyImageInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBlitImage2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr BlitImageInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr BlitImageInfo2KHR -> IO ()

-- | vkCmdBlitImage2KHR - Copy regions of an image, potentially performing
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
-- -   #VUID-vkCmdBlitImage2KHR-commandBuffer-01834# If @commandBuffer@ is
--     an unprotected command buffer, then @srcImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdBlitImage2KHR-commandBuffer-01835# If @commandBuffer@ is
--     an unprotected command buffer, then @dstImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdBlitImage2KHR-commandBuffer-01836# If @commandBuffer@ is
--     a protected command buffer, then @dstImage@ /must/ not be an
--     unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBlitImage2KHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBlitImage2KHR-pBlitImageInfo-parameter# @pBlitImageInfo@
--     /must/ be a valid pointer to a valid 'BlitImageInfo2KHR' structure
--
-- -   #VUID-vkCmdBlitImage2KHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBlitImage2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBlitImage2KHR-renderpass# This command /must/ only be
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'BlitImageInfo2KHR', 'Vulkan.Core10.Handles.CommandBuffer'
cmdBlitImage2KHR :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is the command buffer into which the command will be
                    -- recorded.
                    CommandBuffer
                 -> -- | @pBlitImageInfo@ is a pointer to a 'BlitImageInfo2KHR' structure
                    -- describing the blit parameters.
                    BlitImageInfo2KHR
                 -> io ()
cmdBlitImage2KHR commandBuffer blitImageInfo = liftIO . evalContT $ do
  let vkCmdBlitImage2KHRPtr = pVkCmdBlitImage2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBlitImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBlitImage2KHR is null" Nothing Nothing
  let vkCmdBlitImage2KHR' = mkVkCmdBlitImage2KHR vkCmdBlitImage2KHRPtr
  pBlitImageInfo <- ContT $ withCStruct (blitImageInfo)
  lift $ traceAroundEvent "vkCmdBlitImage2KHR" (vkCmdBlitImage2KHR' (commandBufferHandle (commandBuffer)) pBlitImageInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBufferToImage2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyBufferToImageInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyBufferToImageInfo2KHR -> IO ()

-- | vkCmdCopyBufferToImage2KHR - Copy data from a buffer into an image
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
-- -   #VUID-vkCmdCopyBufferToImage2KHR-commandBuffer-01828# If
--     @commandBuffer@ is an unprotected command buffer, then @srcBuffer@
--     /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyBufferToImage2KHR-commandBuffer-01829# If
--     @commandBuffer@ is an unprotected command buffer, then @dstImage@
--     /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyBufferToImage2KHR-commandBuffer-01830# If
--     @commandBuffer@ is a protected command buffer, then @dstImage@
--     /must/ not be an unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyBufferToImage2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyBufferToImage2KHR-pCopyBufferToImageInfo-parameter#
--     @pCopyBufferToImageInfo@ /must/ be a valid pointer to a valid
--     'CopyBufferToImageInfo2KHR' structure
--
-- -   #VUID-vkCmdCopyBufferToImage2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyBufferToImage2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyBufferToImage2KHR-renderpass# This command /must/
--     only be called outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyBufferToImageInfo2KHR'
cmdCopyBufferToImage2KHR :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @pCopyBufferToImageInfo@ is a pointer to a 'CopyBufferToImageInfo2KHR'
                            -- structure describing the copy parameters.
                            CopyBufferToImageInfo2KHR
                         -> io ()
cmdCopyBufferToImage2KHR commandBuffer copyBufferToImageInfo = liftIO . evalContT $ do
  let vkCmdCopyBufferToImage2KHRPtr = pVkCmdCopyBufferToImage2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyBufferToImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBufferToImage2KHR is null" Nothing Nothing
  let vkCmdCopyBufferToImage2KHR' = mkVkCmdCopyBufferToImage2KHR vkCmdCopyBufferToImage2KHRPtr
  pCopyBufferToImageInfo <- ContT $ withCStruct (copyBufferToImageInfo)
  lift $ traceAroundEvent "vkCmdCopyBufferToImage2KHR" (vkCmdCopyBufferToImage2KHR' (commandBufferHandle (commandBuffer)) pCopyBufferToImageInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImageToBuffer2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyImageToBufferInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyImageToBufferInfo2KHR -> IO ()

-- | vkCmdCopyImageToBuffer2KHR - Copy image data into a buffer
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
-- -   #VUID-vkCmdCopyImageToBuffer2KHR-commandBuffer-01831# If
--     @commandBuffer@ is an unprotected command buffer, then @srcImage@
--     /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyImageToBuffer2KHR-commandBuffer-01832# If
--     @commandBuffer@ is an unprotected command buffer, then @dstBuffer@
--     /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyImageToBuffer2KHR-commandBuffer-01833# If
--     @commandBuffer@ is a protected command buffer, then @dstBuffer@
--     /must/ not be an unprotected buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyImageToBuffer2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyImageToBuffer2KHR-pCopyImageToBufferInfo-parameter#
--     @pCopyImageToBufferInfo@ /must/ be a valid pointer to a valid
--     'CopyImageToBufferInfo2KHR' structure
--
-- -   #VUID-vkCmdCopyImageToBuffer2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyImageToBuffer2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyImageToBuffer2KHR-renderpass# This command /must/
--     only be called outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyImageToBufferInfo2KHR'
cmdCopyImageToBuffer2KHR :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @pCopyImageToBufferInfo@ is a pointer to a 'cmdCopyImageToBuffer2KHR'
                            -- structure describing the copy parameters.
                            CopyImageToBufferInfo2KHR
                         -> io ()
cmdCopyImageToBuffer2KHR commandBuffer copyImageToBufferInfo = liftIO . evalContT $ do
  let vkCmdCopyImageToBuffer2KHRPtr = pVkCmdCopyImageToBuffer2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyImageToBuffer2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImageToBuffer2KHR is null" Nothing Nothing
  let vkCmdCopyImageToBuffer2KHR' = mkVkCmdCopyImageToBuffer2KHR vkCmdCopyImageToBuffer2KHRPtr
  pCopyImageToBufferInfo <- ContT $ withCStruct (copyImageToBufferInfo)
  lift $ traceAroundEvent "vkCmdCopyImageToBuffer2KHR" (vkCmdCopyImageToBuffer2KHR' (commandBufferHandle (commandBuffer)) pCopyImageToBufferInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResolveImage2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr ResolveImageInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr ResolveImageInfo2KHR -> IO ()

-- | vkCmdResolveImage2KHR - Resolve regions of an image
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
-- -   #VUID-vkCmdResolveImage2KHR-commandBuffer-01837# If @commandBuffer@
--     is an unprotected command buffer, then @srcImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdResolveImage2KHR-commandBuffer-01838# If @commandBuffer@
--     is an unprotected command buffer, then @dstImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdResolveImage2KHR-commandBuffer-01839# If @commandBuffer@
--     is a protected command buffer, then @dstImage@ /must/ not be an
--     unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdResolveImage2KHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdResolveImage2KHR-pResolveImageInfo-parameter#
--     @pResolveImageInfo@ /must/ be a valid pointer to a valid
--     'ResolveImageInfo2KHR' structure
--
-- -   #VUID-vkCmdResolveImage2KHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdResolveImage2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdResolveImage2KHR-renderpass# This command /must/ only be
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'ResolveImageInfo2KHR'
cmdResolveImage2KHR :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command will be
                       -- recorded.
                       CommandBuffer
                    -> -- | @pResolveImageInfo@ is a pointer to a 'ResolveImageInfo2KHR' structure
                       -- describing the resolve parameters.
                       ResolveImageInfo2KHR
                    -> io ()
cmdResolveImage2KHR commandBuffer resolveImageInfo = liftIO . evalContT $ do
  let vkCmdResolveImage2KHRPtr = pVkCmdResolveImage2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdResolveImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResolveImage2KHR is null" Nothing Nothing
  let vkCmdResolveImage2KHR' = mkVkCmdResolveImage2KHR vkCmdResolveImage2KHRPtr
  pResolveImageInfo <- ContT $ withCStruct (resolveImageInfo)
  lift $ traceAroundEvent "vkCmdResolveImage2KHR" (vkCmdResolveImage2KHR' (commandBufferHandle (commandBuffer)) pResolveImageInfo)
  pure $ ()


-- | VkBufferCopy2KHR - Structure specifying a buffer copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkBufferCopy2KHR-size-01988# The @size@ /must/ be greater than
--     @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferCopy2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COPY_2_KHR'
--
-- -   #VUID-VkBufferCopy2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- 'CopyBufferInfo2KHR', 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferCopy2KHR = BufferCopy2KHR
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
deriving instance Generic (BufferCopy2KHR)
#endif
deriving instance Show BufferCopy2KHR

instance ToCStruct BufferCopy2KHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCopy2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COPY_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (srcOffset)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (dstOffset)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COPY_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferCopy2KHR where
  peekCStruct p = do
    srcOffset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    dstOffset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ BufferCopy2KHR
             srcOffset dstOffset size

instance Storable BufferCopy2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCopy2KHR where
  zero = BufferCopy2KHR
           zero
           zero
           zero


-- | VkImageCopy2KHR - Structure specifying an image copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkImageCopy2KHR-extent-00140# The number of slices of the
--     @extent@ (for 3D) or layers of the @srcSubresource@ (for non-3D)
--     /must/ match the number of slices of the @extent@ (for 3D) or layers
--     of the @dstSubresource@ (for non-3D)
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageCopy2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COPY_2_KHR'
--
-- -   #VUID-VkImageCopy2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkImageCopy2KHR-srcSubresource-parameter# @srcSubresource@
--     /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- -   #VUID-VkImageCopy2KHR-dstSubresource-parameter# @dstSubresource@
--     /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'CopyImageInfo2KHR', 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageCopy2KHR = ImageCopy2KHR
  { -- | @srcSubresource@ and @dstSubresource@ are
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' structures
    -- specifying the image subresources of the images used for the source and
    -- destination image data, respectively.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
    -- in texels of the sub-regions of the source and destination image data.
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageCopy2KHR" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy2KHR" "dstOffset"
    dstOffset :: Offset3D
  , -- | @extent@ is the size in texels of the image to copy in @width@, @height@
    -- and @depth@.
    extent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCopy2KHR)
#endif
deriving instance Show ImageCopy2KHR

instance ToCStruct ImageCopy2KHR where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCopy2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COPY_2_KHR)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COPY_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageCopy2KHR where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 60 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 72 :: Ptr Extent3D))
    pure $ ImageCopy2KHR
             srcSubresource srcOffset dstSubresource dstOffset extent

instance Storable ImageCopy2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCopy2KHR where
  zero = ImageCopy2KHR
           zero
           zero
           zero
           zero
           zero


-- | VkImageBlit2KHR - Structure specifying an image blit operation
--
-- = Description
--
-- For each element of the @pRegions@ array, a blit operation is performed
-- for the specified source and destination regions.
--
-- == Valid Usage
--
-- -   #VUID-VkImageBlit2KHR-aspectMask-00238# The @aspectMask@ member of
--     @srcSubresource@ and @dstSubresource@ /must/ match
--
-- -   #VUID-VkImageBlit2KHR-layerCount-00239# The @layerCount@ member of
--     @srcSubresource@ and @dstSubresource@ /must/ match
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageBlit2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_BLIT_2_KHR'
--
-- -   #VUID-VkImageBlit2KHR-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--
-- -   #VUID-VkImageBlit2KHR-sType-unique# The @sType@ value of each struct
--     in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkImageBlit2KHR-srcSubresource-parameter# @srcSubresource@
--     /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- -   #VUID-VkImageBlit2KHR-dstSubresource-parameter# @dstSubresource@
--     /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'BlitImageInfo2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageBlit2KHR (es :: [Type]) = ImageBlit2KHR
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
deriving instance Generic (ImageBlit2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageBlit2KHR es)

instance Extensible ImageBlit2KHR where
  extensibleTypeName = "ImageBlit2KHR"
  setNext x next = x{next = next}
  getNext ImageBlit2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageBlit2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CopyCommandTransformInfoQCOM = Just f
    | otherwise = Nothing

instance (Extendss ImageBlit2KHR es, PokeChain es) => ToCStruct (ImageBlit2KHR es) where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageBlit2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_BLIT_2_KHR)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_BLIT_2_KHR)
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

instance (Extendss ImageBlit2KHR es, PeekChain es) => FromCStruct (ImageBlit2KHR es) where
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
    pure $ ImageBlit2KHR
             next srcSubresource ((srcOffsets0, srcOffsets1)) dstSubresource ((dstOffsets0, dstOffsets1))

instance es ~ '[] => Zero (ImageBlit2KHR es) where
  zero = ImageBlit2KHR
           ()
           zero
           (zero, zero)
           zero
           (zero, zero)


-- | VkBufferImageCopy2KHR - Structure specifying a buffer image copy
-- operation
--
-- = Description
--
-- This structure is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy', but adds @sType@
-- and @pNext@ parameters, allowing it to be more easily extended.
--
-- == Valid Usage
--
-- -   #VUID-VkBufferImageCopy2KHR-bufferRowLength-00195# @bufferRowLength@
--     /must/ be @0@, or greater than or equal to the @width@ member of
--     @imageExtent@
--
-- -   #VUID-VkBufferImageCopy2KHR-bufferImageHeight-00196#
--     @bufferImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   #VUID-VkBufferImageCopy2KHR-aspectMask-00212# The @aspectMask@
--     member of @imageSubresource@ /must/ only have a single bit set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferImageCopy2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR'
--
-- -   #VUID-VkBufferImageCopy2KHR-pNext-pNext# @pNext@ /must/ be @NULL@ or
--     a pointer to a valid instance of
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--
-- -   #VUID-VkBufferImageCopy2KHR-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBufferImageCopy2KHR-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'CopyBufferToImageInfo2KHR', 'CopyImageToBufferInfo2KHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferImageCopy2KHR (es :: [Type]) = BufferImageCopy2KHR
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
  , -- No documentation found for Nested "VkBufferImageCopy2KHR" "bufferImageHeight"
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
deriving instance Generic (BufferImageCopy2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BufferImageCopy2KHR es)

instance Extensible BufferImageCopy2KHR where
  extensibleTypeName = "BufferImageCopy2KHR"
  setNext x next = x{next = next}
  getNext BufferImageCopy2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BufferImageCopy2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CopyCommandTransformInfoQCOM = Just f
    | otherwise = Nothing

instance (Extendss BufferImageCopy2KHR es, PokeChain es) => ToCStruct (BufferImageCopy2KHR es) where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferImageCopy2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr Offset3D)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Extent3D)) (zero)
    lift $ f

instance (Extendss BufferImageCopy2KHR es, PeekChain es) => FromCStruct (BufferImageCopy2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    bufferOffset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    bufferRowLength <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    bufferImageHeight <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 48 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 60 :: Ptr Extent3D))
    pure $ BufferImageCopy2KHR
             next bufferOffset bufferRowLength bufferImageHeight imageSubresource imageOffset imageExtent

instance es ~ '[] => Zero (BufferImageCopy2KHR es) where
  zero = BufferImageCopy2KHR
           ()
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkImageResolve2KHR - Structure specifying an image resolve operation
--
-- == Valid Usage
--
-- -   #VUID-VkImageResolve2KHR-aspectMask-00266# The @aspectMask@ member
--     of @srcSubresource@ and @dstSubresource@ /must/ only contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageResolve2KHR-layerCount-00267# The @layerCount@ member
--     of @srcSubresource@ and @dstSubresource@ /must/ match
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageResolve2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR'
--
-- -   #VUID-VkImageResolve2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkImageResolve2KHR-srcSubresource-parameter# @srcSubresource@
--     /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- -   #VUID-VkImageResolve2KHR-dstSubresource-parameter# @dstSubresource@
--     /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D', 'ResolveImageInfo2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageResolve2KHR = ImageResolve2KHR
  { -- | @srcSubresource@ and @dstSubresource@ are
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' structures
    -- specifying the image subresources of the images used for the source and
    -- destination image data, respectively. Resolve of depth\/stencil images
    -- is not supported.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
    -- in texels of the sub-regions of the source and destination image data.
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageResolve2KHR" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve2KHR" "dstOffset"
    dstOffset :: Offset3D
  , -- | @extent@ is the size in texels of the source image to resolve in
    -- @width@, @height@ and @depth@.
    extent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageResolve2KHR)
#endif
deriving instance Show ImageResolve2KHR

instance ToCStruct ImageResolve2KHR where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageResolve2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageResolve2KHR where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 60 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 72 :: Ptr Extent3D))
    pure $ ImageResolve2KHR
             srcSubresource srcOffset dstSubresource dstOffset extent

instance Storable ImageResolve2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageResolve2KHR where
  zero = ImageResolve2KHR
           zero
           zero
           zero
           zero
           zero


-- | VkCopyBufferInfo2KHR - Structure specifying parameters of a buffer copy
-- command
--
-- = Description
--
-- Members defined by this structure with the same name as parameters in
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBuffer' have the identical
-- effect to those parameters; the child structure 'BufferCopy2KHR' is a
-- variant of 'Vulkan.Core10.CommandBufferBuilding.BufferCopy' which
-- includes @sType@ and @pNext@ parameters, allowing it to be extended.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyBufferInfo2KHR-srcOffset-00113# The @srcOffset@ member
--     of each element of @pRegions@ /must/ be less than the size of
--     @srcBuffer@
--
-- -   #VUID-VkCopyBufferInfo2KHR-dstOffset-00114# The @dstOffset@ member
--     of each element of @pRegions@ /must/ be less than the size of
--     @dstBuffer@
--
-- -   #VUID-VkCopyBufferInfo2KHR-size-00115# The @size@ member of each
--     element of @pRegions@ /must/ be less than or equal to the size of
--     @srcBuffer@ minus @srcOffset@
--
-- -   #VUID-VkCopyBufferInfo2KHR-size-00116# The @size@ member of each
--     element of @pRegions@ /must/ be less than or equal to the size of
--     @dstBuffer@ minus @dstOffset@
--
-- -   #VUID-VkCopyBufferInfo2KHR-pRegions-00117# The union of the source
--     regions, and the union of the destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkCopyBufferInfo2KHR-srcBuffer-00118# @srcBuffer@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkCopyBufferInfo2KHR-srcBuffer-00119# If @srcBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyBufferInfo2KHR-dstBuffer-00120# @dstBuffer@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkCopyBufferInfo2KHR-dstBuffer-00121# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyBufferInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR'
--
-- -   #VUID-VkCopyBufferInfo2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyBufferInfo2KHR-srcBuffer-parameter# @srcBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkCopyBufferInfo2KHR-dstBuffer-parameter# @dstBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkCopyBufferInfo2KHR-pRegions-parameter# @pRegions@ /must/ be
--     a valid pointer to an array of @regionCount@ valid 'BufferCopy2KHR'
--     structures
--
-- -   #VUID-VkCopyBufferInfo2KHR-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkCopyBufferInfo2KHR-commonparent# Both of @dstBuffer@, and
--     @srcBuffer@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'BufferCopy2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdCopyBuffer2KHR'
data CopyBufferInfo2KHR = CopyBufferInfo2KHR
  { -- | @srcBuffer@ is the source buffer.
    srcBuffer :: Buffer
  , -- | @dstBuffer@ is the destination buffer.
    dstBuffer :: Buffer
  , -- | @pRegions@ is a pointer to an array of 'BufferCopy2KHR' structures
    -- specifying the regions to copy.
    regions :: Vector BufferCopy2KHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyBufferInfo2KHR)
#endif
deriving instance Show CopyBufferInfo2KHR

instance ToCStruct CopyBufferInfo2KHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyBufferInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (srcBuffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr Buffer)) (dstBuffer)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @BufferCopy2KHR ((Data.Vector.length (regions)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (40 * (i)) :: Ptr BufferCopy2KHR) (e)) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr BufferCopy2KHR))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    f

instance FromCStruct CopyBufferInfo2KHR where
  peekCStruct p = do
    srcBuffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    dstBuffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    regionCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pRegions <- peek @(Ptr BufferCopy2KHR) ((p `plusPtr` 40 :: Ptr (Ptr BufferCopy2KHR)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @BufferCopy2KHR ((pRegions `advancePtrBytes` (40 * (i)) :: Ptr BufferCopy2KHR)))
    pure $ CopyBufferInfo2KHR
             srcBuffer dstBuffer pRegions'

instance Zero CopyBufferInfo2KHR where
  zero = CopyBufferInfo2KHR
           zero
           zero
           mempty


-- | VkCopyImageInfo2KHR - Structure specifying parameters of an image copy
-- command
--
-- == Valid Usage
--
-- -   #VUID-VkCopyImageInfo2KHR-pRegions-00124# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01995# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-00126# @srcImage@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01546# If @srcImage@ is
--     non-sparse then the image or /disjoint/ plane to be copied /must/ be
--     bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImageLayout-00128# @srcImageLayout@
--     /must/ specify the layout of the image subresources of @srcImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImageLayout-01917# @srcImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01996# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-00131# @dstImage@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01547# If @dstImage@ is
--     non-sparse then the image or /disjoint/ plane that is the
--     destination of the copy /must/ be bound completely and contiguously
--     to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImageLayout-00133# @dstImageLayout@
--     /must/ specify the layout of the image subresources of @dstImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImageLayout-01395# @dstImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01548# If the
--     'Vulkan.Core10.Enums.Format.Format' of each of @srcImage@ and
--     @dstImage@ is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     the 'Vulkan.Core10.Enums.Format.Format' of each of @srcImage@ and
--     @dstImage@ /must/ be compatible, as defined
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-images-format-compatibility above>
--
-- -   #VUID-VkCopyImageInfo2KHR-None-01549# In a copy to or from a plane
--     of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image>,
--     the 'Vulkan.Core10.Enums.Format.Format' of the image and plane
--     /must/ be compatible according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes the description of compatible planes>
--     for the plane being copied
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-00136# The sample count of
--     @srcImage@ and @dstImage@ /must/ match
--
-- -   #VUID-VkCopyImageInfo2KHR-srcSubresource-01696# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkCopyImageInfo2KHR-dstSubresource-01697# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyImageInfo2KHR-srcSubresource-01698# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-VkCopyImageInfo2KHR-dstSubresource-01699# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkCopyImageInfo2KHR-srcOffset-01783# The @srcOffset@ and
--     @extent@ members of each element of @pRegions@ /must/ respect the
--     image transfer granularity requirements of @commandBuffer@’s command
--     pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-VkCopyImageInfo2KHR-dstOffset-01784# The @dstOffset@ and
--     @extent@ members of each element of @pRegions@ /must/ respect the
--     image transfer granularity requirements of @commandBuffer@’s command
--     pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-02542# @dstImage@ and @srcImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01551# If neither @srcImage@ nor
--     @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     then for each element of @pRegions@, @srcSubresource.aspectMask@ and
--     @dstSubresource.aspectMask@ /must/ match
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01552# If @srcImage@ has a
--     'Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion two planes>
--     then for each element of @pRegions@, @srcSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01553# If @srcImage@ has a
--     'Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion three planes>
--     then for each element of @pRegions@, @srcSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01554# If @dstImage@ has a
--     'Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion two planes>
--     then for each element of @pRegions@, @dstSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01555# If @dstImage@ has a
--     'Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion three planes>
--     then for each element of @pRegions@, @dstSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01556# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     and the @dstImage@ does not have a multi-planar image format, then
--     for each element of @pRegions@, @dstSubresource.aspectMask@ /must/
--     be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01557# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     and the @srcImage@ does not have a multi-planar image format, then
--     for each element of @pRegions@, @srcSubresource.aspectMask@ /must/
--     be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-04443# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @srcSubresource.baseArrayLayer@ /must/ be @0@ and and
--     @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-04444# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and and
--     @dstSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2KHR-aspectMask-00142# For each element of
--     @pRegions@, @srcSubresource.aspectMask@ /must/ specify aspects
--     present in @srcImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-aspectMask-00143# For each element of
--     @pRegions@, @dstSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcOffset-00144# For each element of
--     @pRegions@, @srcOffset.x@ and (@extent.width@ + @srcOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcOffset-00145# For each element of
--     @pRegions@, @srcOffset.y@ and (@extent.height@ + @srcOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-00146# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcOffset-00147# For each element of
--     @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01785# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01786# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01787# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01788# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01790# If @srcImage@ and
--     @dstImage@ are both of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @extent.depth@ /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01791# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each
--     element of @pRegions@, @extent.depth@ /must/ equal
--     @srcSubresource.layerCount@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01792# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each
--     element of @pRegions@, @extent.depth@ /must/ equal
--     @dstSubresource.layerCount@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstOffset-00150# For each element of
--     @pRegions@, @dstOffset.x@ and (@extent.width@ + @dstOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstOffset-00151# For each element of
--     @pRegions@, @dstOffset.y@ and (@extent.height@ + @dstOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-00152# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstOffset-00153# For each element of
--     @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01727# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, all members of @srcOffset@
--     /must/ be a multiple of the corresponding dimensions of the
--     compressed texel block
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01728# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.width@ /must/ be a
--     multiple of the compressed texel block width or (@extent.width@ +
--     @srcOffset.x@) /must/ equal the width of the specified
--     @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01729# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.height@ /must/ be a
--     multiple of the compressed texel block height or (@extent.height@ +
--     @srcOffset.y@) /must/ equal the height of the specified
--     @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-01730# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@extent.depth@ +
--     @srcOffset.z@) /must/ equal the depth of the specified
--     @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01731# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, all members of @dstOffset@
--     /must/ be a multiple of the corresponding dimensions of the
--     compressed texel block
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01732# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.width@ /must/ be a
--     multiple of the compressed texel block width or (@extent.width@ +
--     @dstOffset.x@) /must/ equal the width of the specified
--     @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01733# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.height@ /must/ be a
--     multiple of the compressed texel block height or (@extent.height@ +
--     @dstOffset.y@) /must/ equal the height of the specified
--     @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-01734# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@extent.depth@ +
--     @dstOffset.z@) /must/ equal the depth of the specified
--     @dstSubresource@ of @dstImage@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyImageInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR'
--
-- -   #VUID-VkCopyImageInfo2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImage-parameter# @srcImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageInfo2KHR-srcImageLayout-parameter# @srcImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImage-parameter# @dstImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageInfo2KHR-dstImageLayout-parameter# @dstImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkCopyImageInfo2KHR-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'ImageCopy2KHR'
--     structures
--
-- -   #VUID-VkCopyImageInfo2KHR-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkCopyImageInfo2KHR-commonparent# Both of @dstImage@, and
--     @srcImage@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Image', 'ImageCopy2KHR',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdCopyImage2KHR'
data CopyImageInfo2KHR = CopyImageInfo2KHR
  { -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the current layout of the source image subresource.
    srcImageLayout :: ImageLayout
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the current layout of the destination image
    -- subresource.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'ImageCopy2KHR' structures
    -- specifying the regions to copy.
    regions :: Vector ImageCopy2KHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageInfo2KHR)
#endif
deriving instance Show CopyImageInfo2KHR

instance ToCStruct CopyImageInfo2KHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @ImageCopy2KHR ((Data.Vector.length (regions)) * 88) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageCopy2KHR) (e)) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr ImageCopy2KHR))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyImageInfo2KHR where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageCopy2KHR) ((p `plusPtr` 48 :: Ptr (Ptr ImageCopy2KHR)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageCopy2KHR ((pRegions `advancePtrBytes` (88 * (i)) :: Ptr ImageCopy2KHR)))
    pure $ CopyImageInfo2KHR
             srcImage srcImageLayout dstImage dstImageLayout pRegions'

instance Zero CopyImageInfo2KHR where
  zero = CopyImageInfo2KHR
           zero
           zero
           zero
           zero
           mempty


-- | VkBlitImageInfo2KHR - Structure specifying parameters of blit image
-- command
--
-- == Valid Usage
--
-- -   #VUID-VkBlitImageInfo2KHR-pRegions-00215# The source region
--     specified by each element of @pRegions@ /must/ be a region that is
--     contained within @srcImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-pRegions-00216# The destination region
--     specified by each element of @pRegions@ /must/ be a region that is
--     contained within @dstImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-pRegions-00217# The union of all
--     destination regions, specified by the elements of @pRegions@, /must/
--     not overlap in memory with any texel that /may/ be sampled during
--     the blit operation
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-01999# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-01561# @srcImage@ /must/ not use
--     a format listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion ???>
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00219# @srcImage@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00220# If @srcImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImageLayout-00221# @srcImageLayout@
--     /must/ specify the layout of the image subresources of @srcImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImageLayout-01398# @srcImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-02000# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_DST_BIT'
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-01562# @dstImage@ /must/ not use
--     a format listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion ???>
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-00224# @dstImage@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-00225# If @dstImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImageLayout-00226# @dstImageLayout@
--     /must/ specify the layout of the image subresources of @dstImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImageLayout-01399# @dstImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00229# If either of @srcImage@ or
--     @dstImage@ was created with a signed integer
--     'Vulkan.Core10.Enums.Format.Format', the other /must/ also have been
--     created with a signed integer 'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00230# If either of @srcImage@ or
--     @dstImage@ was created with an unsigned integer
--     'Vulkan.Core10.Enums.Format.Format', the other /must/ also have been
--     created with an unsigned integer 'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00231# If either of @srcImage@ or
--     @dstImage@ was created with a depth\/stencil format, the other
--     /must/ have exactly the same format
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00232# If @srcImage@ was created
--     with a depth\/stencil format, @filter@ /must/ be
--     'Vulkan.Core10.Enums.Filter.FILTER_NEAREST'
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00233# @srcImage@ /must/ have
--     been created with a @samples@ value of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-00234# @dstImage@ /must/ have
--     been created with a @samples@ value of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkBlitImageInfo2KHR-filter-02001# If @filter@ is
--     'Vulkan.Core10.Enums.Filter.FILTER_LINEAR', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-VkBlitImageInfo2KHR-filter-02002# If @filter@ is
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-VkBlitImageInfo2KHR-filter-00237# If @filter@ is
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT', @srcImage@
--     /must/ be of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkBlitImageInfo2KHR-srcSubresource-01705# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkBlitImageInfo2KHR-dstSubresource-01706# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkBlitImageInfo2KHR-srcSubresource-01707# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-VkBlitImageInfo2KHR-dstSubresource-01708# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-02545# @dstImage@ and @srcImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00240# If either @srcImage@ or
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     then for each element of @pRegions@, @srcSubresource.baseArrayLayer@
--     and @dstSubresource.baseArrayLayer@ /must/ each be @0@, and
--     @srcSubresource.layerCount@ and @dstSubresource.layerCount@ /must/
--     each be @1@.
--
-- -   #VUID-VkBlitImageInfo2KHR-aspectMask-00241# For each element of
--     @pRegions@, @srcSubresource.aspectMask@ /must/ specify aspects
--     present in @srcImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-aspectMask-00242# For each element of
--     @pRegions@, @dstSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-srcOffset-00243# For each element of
--     @pRegions@, @srcOffset@[0].x and @srcOffset@[1].x /must/ both be
--     greater than or equal to @0@ and less than or equal to the width of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-srcOffset-00244# For each element of
--     @pRegions@, @srcOffset@[0].y and @srcOffset@[1].y /must/ both be
--     greater than or equal to @0@ and less than or equal to the height of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00245# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset@[0].y /must/ be @0@ and @srcOffset@[1].y
--     /must/ be @1@
--
-- -   #VUID-VkBlitImageInfo2KHR-srcOffset-00246# For each element of
--     @pRegions@, @srcOffset@[0].z and @srcOffset@[1].z /must/ both be
--     greater than or equal to @0@ and less than or equal to the depth of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-00247# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset@[0].z /must/ be @0@ and @srcOffset@[1].z
--     /must/ be @1@
--
-- -   #VUID-VkBlitImageInfo2KHR-dstOffset-00248# For each element of
--     @pRegions@, @dstOffset@[0].x and @dstOffset@[1].x /must/ both be
--     greater than or equal to @0@ and less than or equal to the width of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-dstOffset-00249# For each element of
--     @pRegions@, @dstOffset@[0].y and @dstOffset@[1].y /must/ both be
--     greater than or equal to @0@ and less than or equal to the height of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-00250# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset@[0].y /must/ be @0@ and @dstOffset@[1].y
--     /must/ be @1@
--
-- -   #VUID-VkBlitImageInfo2KHR-dstOffset-00251# For each element of
--     @pRegions@, @dstOffset@[0].z and @dstOffset@[1].z /must/ both be
--     greater than or equal to @0@ and less than or equal to the depth of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-00252# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset@[0].z /must/ be @0@ and @dstOffset@[1].z
--     /must/ be @1@
--
-- -   #VUID-VkBlitImageInfo2KHR-pRegions-04561# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then @srcImage@ and @dstImage@ /must/ not be a
--     block-compressed image.
--
-- -   #VUID-VkBlitImageInfo2KHR-pRegions-04562# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then the @srcImage@ /must/ be of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and /must/ not be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBlitImageInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR'
--
-- -   #VUID-VkBlitImageInfo2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImage-parameter# @srcImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkBlitImageInfo2KHR-srcImageLayout-parameter# @srcImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImage-parameter# @dstImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkBlitImageInfo2KHR-dstImageLayout-parameter# @dstImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkBlitImageInfo2KHR-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'ImageBlit2KHR'
--     structures
--
-- -   #VUID-VkBlitImageInfo2KHR-filter-parameter# @filter@ /must/ be a
--     valid 'Vulkan.Core10.Enums.Filter.Filter' value
--
-- -   #VUID-VkBlitImageInfo2KHR-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkBlitImageInfo2KHR-commonparent# Both of @dstImage@, and
--     @srcImage@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Filter.Filter', 'Vulkan.Core10.Handles.Image',
-- 'ImageBlit2KHR', 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdBlitImage2KHR'
data BlitImageInfo2KHR = BlitImageInfo2KHR
  { -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- blit.
    srcImageLayout :: ImageLayout
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the blit.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'ImageBlit2KHR' structures
    -- specifying the regions to blit.
    regions :: Vector (SomeStruct ImageBlit2KHR)
  , -- | @filter@ is a 'Vulkan.Core10.Enums.Filter.Filter' specifying the filter
    -- to apply if the blits require scaling.
    filter' :: Filter
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BlitImageInfo2KHR)
#endif
deriving instance Show BlitImageInfo2KHR

instance ToCStruct BlitImageInfo2KHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BlitImageInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @(ImageBlit2KHR _) ((Data.Vector.length (regions)) * 96) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (96 * (i)) :: Ptr (ImageBlit2KHR _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (ImageBlit2KHR _)))) (pPRegions')
    lift $ poke ((p `plusPtr` 56 :: Ptr Filter)) (filter')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Filter)) (zero)
    f

instance FromCStruct BlitImageInfo2KHR where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr (ImageBlit2KHR _)) ((p `plusPtr` 48 :: Ptr (Ptr (ImageBlit2KHR _))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (96 * (i)) :: Ptr (ImageBlit2KHR _)))))
    filter' <- peek @Filter ((p `plusPtr` 56 :: Ptr Filter))
    pure $ BlitImageInfo2KHR
             srcImage srcImageLayout dstImage dstImageLayout pRegions' filter'

instance Zero BlitImageInfo2KHR where
  zero = BlitImageInfo2KHR
           zero
           zero
           zero
           zero
           mempty
           zero


-- | VkCopyBufferToImageInfo2KHR - Structure specifying parameters of a
-- buffer to image copy command
--
-- == Valid Usage
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-04565# If the image
--     region specified by each element of @pRegions@ does not contain
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, it /must/ be a region that is contained within
--     @dstImage@ if the @dstImage@’s 'Vulkan.Core10.Enums.Format.Format'
--     is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     and /must/ be a region that is contained within the plane being
--     copied to if the @dstImage@’s 'Vulkan.Core10.Enums.Format.Format' is
--     a multi-planar format
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-04554# If the image
--     region specified by each element of @pRegions@ does contain
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, the rotated destination region as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-rotation-addressing>
--     /must/ be contained within @dstImage@.
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-04555# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then the @dstImage@ /must/ not be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>.
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-04556# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then the @dstImage@ /must/ be of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and /must/ not be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>.
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-00171# @srcBuffer@ /must/
--     be large enough to contain all buffer locations that are accessed
--     according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-00173# The union of all
--     source regions, and the union of all destination regions, specified
--     by the elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-srcBuffer-00174# @srcBuffer@
--     /must/ have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImage-01997# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-srcBuffer-00176# If @srcBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImage-00177# @dstImage@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImage-00178# If @dstImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImage-00179# @dstImage@ /must/
--     have a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImageLayout-00180#
--     @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImageLayout-01396#
--     @dstImageLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageSubresource-01701# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageSubresource-01702# The
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageOffset-01793# The
--     @imageOffset@ and @imageExtent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of @commandBuffer@’s command pool’s queue family, as
--     described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImage-02543# @dstImage@ /must/
--     not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-commandBuffer-04052# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', the
--     @bufferOffset@ member of any element of @pRegions@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImage-04053# If @dstImage@ has
--     a depth\/stencil format, the @bufferOffset@ member of any element of
--     @pRegions@ /must/ be a multiple of @4@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-commandBuffer-04477# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', for each
--     element of @pRegions@, the @aspectMask@ member of @imageSubresource@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'.
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageOffset-00197# For each
--     element of @pRegions@ not containing
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, , @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @dstImage@ where this refers to the width of the /plane/ of the
--     image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageOffset-00198# For each
--     element of @pRegions@ not containing
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, , @imageOffset.y@ and (imageExtent.height +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @dstImage@ where this refers to the height of the /plane/ of the
--     image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-bufferOffset-01558# If @dstImage@
--     does not have either a depth\/stencil or a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the format’s texel block size
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-bufferOffset-01559# If @dstImage@
--     has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the element size of the compatible format for the format
--     and the @aspectMask@ of the @imageSubresource@ as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes ???>
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-srcImage-00199# If @dstImage@ is
--     of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageOffset-00200# For each
--     element of @pRegions@, @imageOffset.z@ and (imageExtent.depth +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-srcImage-00201# If @dstImage@ is
--     of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-bufferRowLength-00203# If
--     @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferRowLength@ /must/ be a
--     multiple of the compressed texel block width
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-bufferImageHeight-00204# If
--     @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferImageHeight@ /must/ be a
--     multiple of the compressed texel block height
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageOffset-00205# If @dstImage@
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, all members of @imageOffset@ /must/
--     be a multiple of the corresponding dimensions of the compressed
--     texel block
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-bufferOffset-00206# If @dstImage@
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferOffset@ /must/ be a multiple
--     of the compressed texel block size in bytes
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageExtent-00207# If @dstImage@
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.width@ /must/ be a
--     multiple of the compressed texel block width or (@imageExtent.width@
--     + @imageOffset.x@) /must/ equal the width of the specified
--     @imageSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageExtent-00208# If @dstImage@
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.height@ /must/ be a
--     multiple of the compressed texel block height or
--     (@imageExtent.height@ + @imageOffset.y@) /must/ equal the height of
--     the specified @imageSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-imageExtent-00209# If @dstImage@
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@imageExtent.depth@
--     + @imageOffset.z@) /must/ equal the depth of the specified
--     @imageSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-aspectMask-00211# For each element
--     of @pRegions@, @imageSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-aspectMask-01560# If @dstImage@
--     has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     (with
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     valid only for image formats with three planes)
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-baseArrayLayer-00213# If
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     for each element of @pRegions@, @imageSubresource.baseArrayLayer@
--     /must/ be @0@ and @imageSubresource.layerCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR'
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-srcBuffer-parameter# @srcBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImage-parameter# @dstImage@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-pRegions-parameter# @pRegions@
--     /must/ be a valid pointer to an array of @regionCount@ valid
--     'BufferImageCopy2KHR' structures
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-regionCount-arraylength#
--     @regionCount@ /must/ be greater than @0@
--
-- -   #VUID-VkCopyBufferToImageInfo2KHR-commonparent# Both of @dstImage@,
--     and @srcBuffer@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'BufferImageCopy2KHR',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyBufferToImage2KHR'
data CopyBufferToImageInfo2KHR = CopyBufferToImageInfo2KHR
  { -- | @srcBuffer@ is the source buffer.
    srcBuffer :: Buffer
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the copy.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'BufferImageCopy2KHR' structures
    -- specifying the regions to copy.
    regions :: Vector (SomeStruct BufferImageCopy2KHR)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyBufferToImageInfo2KHR)
#endif
deriving instance Show CopyBufferToImageInfo2KHR

instance ToCStruct CopyBufferToImageInfo2KHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyBufferToImageInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (srcBuffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @(BufferImageCopy2KHR _) ((Data.Vector.length (regions)) * 72) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (72 * (i)) :: Ptr (BufferImageCopy2KHR _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (BufferImageCopy2KHR _)))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyBufferToImageInfo2KHR where
  peekCStruct p = do
    srcBuffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    dstImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pRegions <- peek @(Ptr (BufferImageCopy2KHR _)) ((p `plusPtr` 40 :: Ptr (Ptr (BufferImageCopy2KHR _))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr (BufferImageCopy2KHR _)))))
    pure $ CopyBufferToImageInfo2KHR
             srcBuffer dstImage dstImageLayout pRegions'

instance Zero CopyBufferToImageInfo2KHR where
  zero = CopyBufferToImageInfo2KHR
           zero
           zero
           zero
           mempty


-- | VkCopyImageToBufferInfo2KHR - Structure specifying parameters of a image
-- to buffer copy command
--
-- == Valid Usage
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-04566# If the image
--     region specified by each element of @pRegions@ does not contain
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, it /must/ be a region that is contained within
--     @srcImage@ if the @srcImage@’s 'Vulkan.Core10.Enums.Format.Format'
--     is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     and /must/ be a region that is contained within the plane being
--     copied if the @srcImage@’s 'Vulkan.Core10.Enums.Format.Format' is a
--     multi-planar format
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-04557# If the image
--     region specified by each element of @pRegions@ does contain
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, the rotated source region as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-rotation-addressing>
--     /must/ be contained within @srcImage@.
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-04558# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then the @srcImage@ /must/ not be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-04559# If any element of
--     @pRegions@ contains
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, then the @srcImage@ /must/ be of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and /must/ not be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>.
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-00183# @dstBuffer@ /must/
--     be large enough to contain all buffer locations that are accessed
--     according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-00184# The union of all
--     source regions, and the union of all destination regions, specified
--     by the elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-00186# @srcImage@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-01998# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-00187# If @srcImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-dstBuffer-00191# @dstBuffer@
--     /must/ have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-dstBuffer-00192# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-00188# @srcImage@ /must/
--     have a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImageLayout-00189#
--     @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImageLayout-01397#
--     @srcImageLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageSubresource-01703# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageSubresource-01704# The
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageOffset-01794# The
--     @imageOffset@ and @imageExtent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of @commandBuffer@’s command pool’s queue family, as
--     described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-02544# @srcImage@ /must/
--     not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-commandBuffer-04054# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', the
--     @bufferOffset@ member of any element of @pRegions@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-04055# If @srcImage@ has
--     a depth\/stencil format, the @bufferOffset@ member of any element of
--     @pRegions@ /must/ be a multiple of @4@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageOffset-00197# For each
--     element of @pRegions@ not containing
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, , @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @srcImage@ where this refers to the width of the /plane/ of the
--     image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageOffset-00198# For each
--     element of @pRegions@ not containing
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'
--     in its @pNext@ chain, , @imageOffset.y@ and (imageExtent.height +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @srcImage@ where this refers to the height of the /plane/ of the
--     image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-bufferOffset-01558# If
--     {imageparam} does not have either a depth\/stencil or a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the format’s texel block size
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-bufferOffset-01559# If
--     {imageparam} has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the element size of the compatible format for the format
--     and the @aspectMask@ of the @imageSubresource@ as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes ???>
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-00199# If {imageparam} is
--     of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageOffset-00200# For each
--     element of @pRegions@, @imageOffset.z@ and (imageExtent.depth +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of {imageparam}
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-00201# If {imageparam} is
--     of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-bufferRowLength-00203# If
--     {imageparam} is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferRowLength@ /must/ be a
--     multiple of the compressed texel block width
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-bufferImageHeight-00204# If
--     {imageparam} is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferImageHeight@ /must/ be a
--     multiple of the compressed texel block height
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageOffset-00205# If {imageparam}
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, all members of @imageOffset@ /must/
--     be a multiple of the corresponding dimensions of the compressed
--     texel block
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-bufferOffset-00206# If
--     {imageparam} is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferOffset@ /must/ be a multiple
--     of the compressed texel block size in bytes
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageExtent-00207# If {imageparam}
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.width@ /must/ be a
--     multiple of the compressed texel block width or (@imageExtent.width@
--     + @imageOffset.x@) /must/ equal the width of the specified
--     @imageSubresource@ of {imageparam}
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageExtent-00208# If {imageparam}
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.height@ /must/ be a
--     multiple of the compressed texel block height or
--     (@imageExtent.height@ + @imageOffset.y@) /must/ equal the height of
--     the specified @imageSubresource@ of {imageparam}
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-imageExtent-00209# If {imageparam}
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@imageExtent.depth@
--     + @imageOffset.z@) /must/ equal the depth of the specified
--     @imageSubresource@ of {imageparam}
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-aspectMask-00211# For each element
--     of @pRegions@, @imageSubresource.aspectMask@ /must/ specify aspects
--     present in {imageparam}
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-aspectMask-01560# If {imageparam}
--     has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     (with
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     valid only for image formats with three planes)
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-baseArrayLayer-00213# If
--     {imageparam} is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element of
--     @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be @0@ and
--     @imageSubresource.layerCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR'
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImage-parameter# @srcImage@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-srcImageLayout-parameter#
--     @srcImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-dstBuffer-parameter# @dstBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-pRegions-parameter# @pRegions@
--     /must/ be a valid pointer to an array of @regionCount@ valid
--     'BufferImageCopy2KHR' structures
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-regionCount-arraylength#
--     @regionCount@ /must/ be greater than @0@
--
-- -   #VUID-VkCopyImageToBufferInfo2KHR-commonparent# Both of @dstBuffer@,
--     and @srcImage@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'BufferImageCopy2KHR',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyImageToBuffer2KHR'
data CopyImageToBufferInfo2KHR = CopyImageToBufferInfo2KHR
  { -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- copy.
    srcImageLayout :: ImageLayout
  , -- | @dstBuffer@ is the destination buffer.
    dstBuffer :: Buffer
  , -- | @pRegions@ is a pointer to an array of 'BufferImageCopy2KHR' structures
    -- specifying the regions to copy.
    regions :: Vector (SomeStruct BufferImageCopy2KHR)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageToBufferInfo2KHR)
#endif
deriving instance Show CopyImageToBufferInfo2KHR

instance ToCStruct CopyImageToBufferInfo2KHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageToBufferInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Buffer)) (dstBuffer)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @(BufferImageCopy2KHR _) ((Data.Vector.length (regions)) * 72) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (72 * (i)) :: Ptr (BufferImageCopy2KHR _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (BufferImageCopy2KHR _)))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Buffer)) (zero)
    f

instance FromCStruct CopyImageToBufferInfo2KHR where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstBuffer <- peek @Buffer ((p `plusPtr` 32 :: Ptr Buffer))
    regionCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pRegions <- peek @(Ptr (BufferImageCopy2KHR _)) ((p `plusPtr` 48 :: Ptr (Ptr (BufferImageCopy2KHR _))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr (BufferImageCopy2KHR _)))))
    pure $ CopyImageToBufferInfo2KHR
             srcImage srcImageLayout dstBuffer pRegions'

instance Zero CopyImageToBufferInfo2KHR where
  zero = CopyImageToBufferInfo2KHR
           zero
           zero
           zero
           mempty


-- | VkResolveImageInfo2KHR - Structure specifying parameters of resolve
-- image command
--
-- == Valid Usage
--
-- -   #VUID-VkResolveImageInfo2KHR-pRegions-00255# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImage-00256# If @srcImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImage-00257# @srcImage@ /must/ have
--     a sample count equal to any valid sample count value other than
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImage-00258# If @dstImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImage-00259# @dstImage@ /must/ have
--     a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImageLayout-00260# @srcImageLayout@
--     /must/ specify the layout of the image subresources of @srcImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImageLayout-01400# @srcImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImageLayout-00262# @dstImageLayout@
--     /must/ specify the layout of the image subresources of @dstImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImageLayout-01401# @dstImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImage-02003# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImage-01386# @srcImage@ and
--     @dstImage@ /must/ have been created with the same image format
--
-- -   #VUID-VkResolveImageInfo2KHR-srcSubresource-01709# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkResolveImageInfo2KHR-dstSubresource-01710# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkResolveImageInfo2KHR-srcSubresource-01711# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-VkResolveImageInfo2KHR-dstSubresource-01712# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImage-02546# @dstImage@ and
--     @srcImage@ /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImage-04446# If either @srcImage@ or
--     @dstImage@ are of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @srcSubresource.baseArrayLayer@ /must/ be @0@ and
--     @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImage-04447# If either @srcImage@ or
--     @dstImage@ are of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and
--     @dstSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkResolveImageInfo2KHR-srcOffset-00269# For each element of
--     @pRegions@, @srcOffset.x@ and (@extent.width@ + @srcOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkResolveImageInfo2KHR-srcOffset-00270# For each element of
--     @pRegions@, @srcOffset.y@ and (@extent.height@ + @srcOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImage-00271# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @srcOffset.y@ /must/ be @0@ and
--     @extent.height@ /must/ be @1@
--
-- -   #VUID-VkResolveImageInfo2KHR-srcOffset-00272# For each element of
--     @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImage-00273# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkResolveImageInfo2KHR-dstOffset-00274# For each element of
--     @pRegions@, @dstOffset.x@ and (@extent.width@ + @dstOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkResolveImageInfo2KHR-dstOffset-00275# For each element of
--     @pRegions@, @dstOffset.y@ and (@extent.height@ + @dstOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImage-00276# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @dstOffset.y@ /must/ be @0@ and
--     @extent.height@ /must/ be @1@
--
-- -   #VUID-VkResolveImageInfo2KHR-dstOffset-00277# For each element of
--     @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImage-00278# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkResolveImageInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR'
--
-- -   #VUID-VkResolveImageInfo2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImage-parameter# @srcImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkResolveImageInfo2KHR-srcImageLayout-parameter#
--     @srcImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImage-parameter# @dstImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkResolveImageInfo2KHR-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkResolveImageInfo2KHR-pRegions-parameter# @pRegions@ /must/
--     be a valid pointer to an array of @regionCount@ valid
--     'ImageResolve2KHR' structures
--
-- -   #VUID-VkResolveImageInfo2KHR-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkResolveImageInfo2KHR-commonparent# Both of @dstImage@, and
--     @srcImage@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'ImageResolve2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdResolveImage2KHR'
data ResolveImageInfo2KHR = ResolveImageInfo2KHR
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
  , -- | @pRegions@ is a pointer to an array of 'ImageResolve2KHR' structures
    -- specifying the regions to resolve.
    regions :: Vector ImageResolve2KHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ResolveImageInfo2KHR)
#endif
deriving instance Show ResolveImageInfo2KHR

instance ToCStruct ResolveImageInfo2KHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ResolveImageInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @ImageResolve2KHR ((Data.Vector.length (regions)) * 88) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageResolve2KHR) (e)) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr ImageResolve2KHR))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct ResolveImageInfo2KHR where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageResolve2KHR) ((p `plusPtr` 48 :: Ptr (Ptr ImageResolve2KHR)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageResolve2KHR ((pRegions `advancePtrBytes` (88 * (i)) :: Ptr ImageResolve2KHR)))
    pure $ ResolveImageInfo2KHR
             srcImage srcImageLayout dstImage dstImageLayout pRegions'

instance Zero ResolveImageInfo2KHR where
  zero = ResolveImageInfo2KHR
           zero
           zero
           zero
           zero
           mempty


type KHR_COPY_COMMANDS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_COPY_COMMANDS_2_SPEC_VERSION"
pattern KHR_COPY_COMMANDS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_COPY_COMMANDS_2_SPEC_VERSION = 1


type KHR_COPY_COMMANDS_2_EXTENSION_NAME = "VK_KHR_copy_commands2"

-- No documentation found for TopLevel "VK_KHR_COPY_COMMANDS_2_EXTENSION_NAME"
pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME = "VK_KHR_copy_commands2"

