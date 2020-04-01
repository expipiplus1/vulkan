{-# language CPP #-}
module Graphics.Vulkan.Core10.CommandBuffer  ( allocateCommandBuffers
                                             , withCommandBuffers
                                             , freeCommandBuffers
                                             , beginCommandBuffer
                                             , useCommandBuffer
                                             , endCommandBuffer
                                             , resetCommandBuffer
                                             , CommandBufferAllocateInfo(..)
                                             , CommandBufferInheritanceInfo(..)
                                             , CommandBufferBeginInfo(..)
                                             ) where

import Control.Exception.Base (bracket)
import Control.Exception.Base (bracket_)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.CStruct.Extends (forgetExtensions)
import Graphics.Vulkan.CStruct.Extends (peekSomeCStruct)
import Graphics.Vulkan.CStruct.Extends (withSomeCStruct)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering (CommandBufferInheritanceConditionalRenderingInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform (CommandBufferInheritanceRenderPassTransformInfoQCOM)
import Graphics.Vulkan.Core10.Enums.CommandBufferLevel (CommandBufferLevel)
import Graphics.Vulkan.Core10.Enums.CommandBufferResetFlagBits (CommandBufferResetFlags)
import Graphics.Vulkan.Core10.Enums.CommandBufferResetFlagBits (CommandBufferResetFlags)
import Graphics.Vulkan.Core10.Enums.CommandBufferResetFlagBits (CommandBufferResetFlagBits(..))
import Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits (CommandBufferUsageFlags)
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Core10.Handles (CommandPool)
import Graphics.Vulkan.Core10.Handles (CommandPool(..))
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkAllocateCommandBuffers))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkBeginCommandBuffer))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkEndCommandBuffer))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkFreeCommandBuffers))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkResetCommandBuffer))
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupCommandBufferBeginInfo)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.Core10.Handles (Framebuffer)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Graphics.Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlags)
import Graphics.Vulkan.Core10.Handles (RenderPass)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.CStruct.Extends (SomeStruct)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateCommandBuffers
  :: FunPtr (Ptr Device_T -> Ptr CommandBufferAllocateInfo -> Ptr (Ptr CommandBuffer_T) -> IO Result) -> Ptr Device_T -> Ptr CommandBufferAllocateInfo -> Ptr (Ptr CommandBuffer_T) -> IO Result

-- | vkAllocateCommandBuffers - Allocate command buffers from an existing
-- command pool
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the command pool.
--
-- -   @pAllocateInfo@ is a pointer to a 'CommandBufferAllocateInfo'
--     structure describing parameters of the allocation.
--
-- -   @pCommandBuffers@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handles in which the
--     resulting command buffer objects are returned. The array /must/ be
--     at least the length specified by the @commandBufferCount@ member of
--     @pAllocateInfo@. Each allocated command buffer begins in the initial
--     state.
--
-- = Description
--
-- 'allocateCommandBuffers' /can/ be used to create multiple command
-- buffers. If the creation of any of those command buffers fails, the
-- implementation /must/ destroy all successfully created command buffer
-- objects from this command, set all entries of the @pCommandBuffers@
-- array to @NULL@ and return the error.
--
-- When command buffers are first allocated, they are in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @pAllocateInfo@ /must/ be a valid pointer to a valid
--     'CommandBufferAllocateInfo' structure
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @pAllocateInfo@::commandBufferCount
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handles
--
-- -   The value referenced by @pAllocateInfo@::@commandBufferCount@ /must/
--     be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @pAllocateInfo@::commandPool /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'CommandBufferAllocateInfo', 'Graphics.Vulkan.Core10.Handles.Device'
allocateCommandBuffers :: Device -> CommandBufferAllocateInfo -> IO (("commandBuffers" ::: Vector CommandBuffer))
allocateCommandBuffers device allocateInfo = evalContT $ do
  let cmds = deviceCmds (device :: Device)
  let vkAllocateCommandBuffers' = mkVkAllocateCommandBuffers (pVkAllocateCommandBuffers cmds)
  pAllocateInfo <- ContT $ withCStruct (allocateInfo)
  pPCommandBuffers <- ContT $ bracket (callocBytes @(Ptr CommandBuffer_T) ((fromIntegral $ commandBufferCount ((allocateInfo) :: CommandBufferAllocateInfo)) * 8)) free
  r <- lift $ vkAllocateCommandBuffers' (deviceHandle (device)) pAllocateInfo (pPCommandBuffers)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCommandBuffers <- lift $ generateM (fromIntegral $ commandBufferCount ((allocateInfo) :: CommandBufferAllocateInfo)) (\i -> do
    pCommandBuffersElem <- peek @(Ptr CommandBuffer_T) ((pPCommandBuffers `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)))
    pure $ (\h -> CommandBuffer h cmds ) pCommandBuffersElem)
  pure $ (pCommandBuffers)

-- | A safe wrapper for 'allocateCommandBuffers' and 'freeCommandBuffers'
-- using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withCommandBuffers :: Device -> CommandBufferAllocateInfo -> (Vector CommandBuffer -> IO r) -> IO r
withCommandBuffers device commandBufferAllocateInfo =
  bracket
    (allocateCommandBuffers device commandBufferAllocateInfo)
    (\o -> freeCommandBuffers device (commandPool (commandBufferAllocateInfo :: CommandBufferAllocateInfo)) o)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeCommandBuffers
  :: FunPtr (Ptr Device_T -> CommandPool -> Word32 -> Ptr (Ptr CommandBuffer_T) -> IO ()) -> Ptr Device_T -> CommandPool -> Word32 -> Ptr (Ptr CommandBuffer_T) -> IO ()

-- | vkFreeCommandBuffers - Free command buffers
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the command pool.
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandPool' is the command pool
--     from which the command buffers were allocated.
--
-- -   @commandBufferCount@ is the length of the @pCommandBuffers@ array.
--
-- -   @pCommandBuffers@ is a pointer to an array of handles of command
--     buffers to free.
--
-- = Description
--
-- Any primary command buffer that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has any element of @pCommandBuffers@ recorded into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   All elements of @pCommandBuffers@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @commandBufferCount@ 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--     handles, each element of which /must/ either be a valid handle or
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandPool' handle
--
-- -   @commandBufferCount@ /must/ be greater than @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandPool' /must/ have been
--     created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   Each element of @pCommandBuffers@ that is a valid handle /must/ have
--     been created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.CommandPool'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandPool' /must/
--     be externally synchronized
--
-- -   Host access to each member of @pCommandBuffers@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandPool',
-- 'Graphics.Vulkan.Core10.Handles.Device'
freeCommandBuffers :: Device -> CommandPool -> ("commandBuffers" ::: Vector CommandBuffer) -> IO ()
freeCommandBuffers device commandPool commandBuffers = evalContT $ do
  let vkFreeCommandBuffers' = mkVkFreeCommandBuffers (pVkFreeCommandBuffers (deviceCmds (device :: Device)))
  pPCommandBuffers <- ContT $ allocaBytesAligned @(Ptr CommandBuffer_T) ((Data.Vector.length (commandBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBuffers `plusPtr` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)) (commandBufferHandle (e))) (commandBuffers)
  lift $ vkFreeCommandBuffers' (deviceHandle (device)) (commandPool) ((fromIntegral (Data.Vector.length $ (commandBuffers)) :: Word32)) (pPCommandBuffers)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBeginCommandBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (CommandBufferBeginInfo a) -> IO Result) -> Ptr CommandBuffer_T -> Ptr (CommandBufferBeginInfo a) -> IO Result

-- | vkBeginCommandBuffer - Start recording a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the handle of the
--     command buffer which is to be put in the recording state.
--
-- -   @pBeginInfo@ points to a 'CommandBufferBeginInfo' structure defining
--     additional information about how the command buffer begins
--     recording.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording or pending state>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     a 'Graphics.Vulkan.Core10.Handles.CommandPool' which did not have
--     the
--     'Graphics.Vulkan.Core10.Enums.CommandPoolCreateFlagBits.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT'
--     flag set, 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be
--     in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a secondary
--     command buffer, the @pInheritanceInfo@ member of @pBeginInfo@ /must/
--     be a valid 'CommandBufferInheritanceInfo' structure
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a secondary
--     command buffer and either the @occlusionQueryEnable@ member of the
--     @pInheritanceInfo@ member of @pBeginInfo@ is
--     'Graphics.Vulkan.Core10.BaseType.FALSE', or the precise occlusion
--     queries feature is not enabled, the @queryFlags@ member of the
--     @pInheritanceInfo@ member @pBeginInfo@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a primary
--     command buffer, then @pBeginInfo->flags@ /must/ not set both the
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT'
--     and the
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flags
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pBeginInfo@ /must/ be a valid pointer to a valid
--     'CommandBufferBeginInfo' structure
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer', 'CommandBufferBeginInfo'
beginCommandBuffer :: PokeChain a => CommandBuffer -> CommandBufferBeginInfo a -> IO ()
beginCommandBuffer commandBuffer beginInfo = evalContT $ do
  let vkBeginCommandBuffer' = mkVkBeginCommandBuffer (pVkBeginCommandBuffer (deviceCmds (commandBuffer :: CommandBuffer)))
  pBeginInfo <- ContT $ withCStruct (beginInfo)
  r <- lift $ vkBeginCommandBuffer' (commandBufferHandle (commandBuffer)) pBeginInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))

-- | A safe wrapper for 'beginCommandBuffer' and 'endCommandBuffer' using
-- 'bracket_'
useCommandBuffer :: PokeChain a => CommandBuffer -> CommandBufferBeginInfo a -> IO r -> IO r
useCommandBuffer commandBuffer commandBufferBeginInfo =
  bracket_
    (beginCommandBuffer commandBuffer commandBufferBeginInfo)
    (endCommandBuffer commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEndCommandBuffer
  :: FunPtr (Ptr CommandBuffer_T -> IO Result) -> Ptr CommandBuffer_T -> IO Result

-- | vkEndCommandBuffer - Finish recording a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     to complete recording.
--
-- = Description
--
-- If there was an error during recording, the application will be notified
-- by an unsuccessful return code returned by 'endCommandBuffer'. If the
-- application wishes to further use the command buffer, the command buffer
-- /must/ be reset. The command buffer /must/ have been in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>,
-- and is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a primary
--     command buffer, there /must/ not be an active render pass instance
--
-- -   All queries made
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--     during the recording of
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ have been made
--     inactive
--
-- -   Conditional rendering must not be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a secondary
--     command buffer, there /must/ not be an outstanding
--     'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.cmdBeginDebugUtilsLabelEXT'
--     command recorded to 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--     that has not previously been ended by a call to
--     'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.cmdEndDebugUtilsLabelEXT'.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a secondary
--     command buffer, there /must/ not be an outstanding
--     'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerBeginEXT'
--     command recorded to 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--     that has not previously been ended by a call to
--     'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerEndEXT'.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
endCommandBuffer :: CommandBuffer -> IO ()
endCommandBuffer commandBuffer = do
  let vkEndCommandBuffer' = mkVkEndCommandBuffer (pVkEndCommandBuffer (deviceCmds (commandBuffer :: CommandBuffer)))
  r <- vkEndCommandBuffer' (commandBufferHandle (commandBuffer))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetCommandBuffer
  :: FunPtr (Ptr CommandBuffer_T -> CommandBufferResetFlags -> IO Result) -> Ptr CommandBuffer_T -> CommandBufferResetFlags -> IO Result

-- | vkResetCommandBuffer - Reset a command buffer to the initial state
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     to reset. The command buffer /can/ be in any state other than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending>,
--     and is moved into the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.CommandBufferResetFlagBits.CommandBufferResetFlagBits'
--     controlling the reset operation.
--
-- = Description
--
-- Any primary command buffer that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has 'Graphics.Vulkan.Core10.Handles.CommandBuffer' recorded into it,
-- becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ have been
--     allocated from a pool that was created with the
--     'Graphics.Vulkan.Core10.Enums.CommandPoolCreateFlagBits.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.CommandBufferResetFlagBits.CommandBufferResetFlagBits'
--     values
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.CommandBufferResetFlagBits.CommandBufferResetFlags'
resetCommandBuffer :: CommandBuffer -> CommandBufferResetFlags -> IO ()
resetCommandBuffer commandBuffer flags = do
  let vkResetCommandBuffer' = mkVkResetCommandBuffer (pVkResetCommandBuffer (deviceCmds (commandBuffer :: CommandBuffer)))
  r <- vkResetCommandBuffer' (commandBufferHandle (commandBuffer)) (flags)
  when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkCommandBufferAllocateInfo - Structure specifying the allocation
-- parameters for command buffer object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.CommandBufferLevel.CommandBufferLevel',
-- 'Graphics.Vulkan.Core10.Handles.CommandPool',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'allocateCommandBuffers'
data CommandBufferAllocateInfo = CommandBufferAllocateInfo
  { -- | 'Graphics.Vulkan.Core10.Handles.CommandPool' /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Handles.CommandPool' handle
    commandPool :: CommandPool
  , -- | @level@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.CommandBufferLevel.CommandBufferLevel'
    -- value
    level :: CommandBufferLevel
  , -- | @commandBufferCount@ /must/ be greater than @0@
    commandBufferCount :: Word32
  }
  deriving (Typeable)
deriving instance Show CommandBufferAllocateInfo

instance ToCStruct CommandBufferAllocateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferAllocateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CommandPool)) (commandPool)
    poke ((p `plusPtr` 24 :: Ptr CommandBufferLevel)) (level)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (commandBufferCount)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CommandPool)) (zero)
    poke ((p `plusPtr` 24 :: Ptr CommandBufferLevel)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct CommandBufferAllocateInfo where
  peekCStruct p = do
    commandPool <- peek @CommandPool ((p `plusPtr` 16 :: Ptr CommandPool))
    level <- peek @CommandBufferLevel ((p `plusPtr` 24 :: Ptr CommandBufferLevel))
    commandBufferCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ CommandBufferAllocateInfo
             commandPool level commandBufferCount

instance Storable CommandBufferAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandBufferAllocateInfo where
  zero = CommandBufferAllocateInfo
           zero
           zero
           zero


-- | VkCommandBufferInheritanceInfo - Structure specifying command buffer
-- inheritance info
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is not enabled, @occlusionQueryEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is enabled, @queryFlags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
--     values
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is not enabled, @queryFlags@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipeline statistics queries>
--     feature is enabled, @pipelineStatistics@ /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits'
--     values
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipeline statistics queries>
--     feature is not enabled, @pipelineStatistics@ /must/ be @0@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.CommandBufferInheritanceConditionalRenderingInfoEXT'
--     or
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Framebuffer', and
--     'Graphics.Vulkan.Core10.Handles.RenderPass' that are valid handles
--     of non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32', 'CommandBufferBeginInfo',
-- 'Graphics.Vulkan.Core10.Handles.Framebuffer',
-- 'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlags',
-- 'Graphics.Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlags',
-- 'Graphics.Vulkan.Core10.Handles.RenderPass',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data CommandBufferInheritanceInfo (es :: [Type]) = CommandBufferInheritanceInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | 'Graphics.Vulkan.Core10.Handles.RenderPass' is a
    -- 'Graphics.Vulkan.Core10.Handles.RenderPass' object defining which render
    -- passes the 'Graphics.Vulkan.Core10.Handles.CommandBuffer' will be
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
    -- with and /can/ be executed within. If the
    -- 'Graphics.Vulkan.Core10.Handles.CommandBuffer' will not be executed
    -- within a render pass instance,
    -- 'Graphics.Vulkan.Core10.Handles.RenderPass' is ignored.
    renderPass :: RenderPass
  , -- | @subpass@ is the index of the subpass within the render pass instance
    -- that the 'Graphics.Vulkan.Core10.Handles.CommandBuffer' will be executed
    -- within. If the 'Graphics.Vulkan.Core10.Handles.CommandBuffer' will not
    -- be executed within a render pass instance, @subpass@ is ignored.
    subpass :: Word32
  , -- | 'Graphics.Vulkan.Core10.Handles.Framebuffer' optionally refers to the
    -- 'Graphics.Vulkan.Core10.Handles.Framebuffer' object that the
    -- 'Graphics.Vulkan.Core10.Handles.CommandBuffer' will be rendering to if
    -- it is executed within a render pass instance. It /can/ be
    -- 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE' if the framebuffer is
    -- not known, or if the 'Graphics.Vulkan.Core10.Handles.CommandBuffer' will
    -- not be executed within a render pass instance.
    --
    -- Note
    --
    -- Specifying the exact framebuffer that the secondary command buffer will
    -- be executed with /may/ result in better performance at command buffer
    -- execution time.
    framebuffer :: Framebuffer
  , -- | @occlusionQueryEnable@ specifies whether the command buffer /can/ be
    -- executed while an occlusion query is active in the primary command
    -- buffer. If this is 'Graphics.Vulkan.Core10.BaseType.TRUE', then this
    -- command buffer /can/ be executed whether the primary command buffer has
    -- an occlusion query active or not. If this is
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE', then the primary command buffer
    -- /must/ not have an occlusion query active.
    occlusionQueryEnable :: Bool
  , -- | @queryFlags@ specifies the query flags that /can/ be used by an active
    -- occlusion query in the primary command buffer when this secondary
    -- command buffer is executed. If this value includes the
    -- 'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT'
    -- bit, then the active query /can/ return boolean results or actual sample
    -- counts. If this bit is not set, then the active query /must/ not use the
    -- 'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT'
    -- bit.
    queryFlags :: QueryControlFlags
  , -- | @pipelineStatistics@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits'
    -- specifying the set of pipeline statistics that /can/ be counted by an
    -- active query in the primary command buffer when this secondary command
    -- buffer is executed. If this value includes a given bit, then this
    -- command buffer /can/ be executed whether the primary command buffer has
    -- a pipeline statistics query active that includes this bit or not. If
    -- this value excludes a given bit, then the active pipeline statistics
    -- query /must/ not be from a query pool that counts that statistic.
    pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (CommandBufferInheritanceInfo es)

instance Extensible CommandBufferInheritanceInfo where
  extensibleType = STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  setNext x next = x{next = next}
  getNext CommandBufferInheritanceInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CommandBufferInheritanceInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CommandBufferInheritanceRenderPassTransformInfoQCOM = Just f
    | Just Refl <- eqT @e @CommandBufferInheritanceConditionalRenderingInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (CommandBufferInheritanceInfo es) where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPass)) (renderPass)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (subpass)
    lift $ poke ((p `plusPtr` 32 :: Ptr Framebuffer)) (framebuffer)
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (occlusionQueryEnable))
    lift $ poke ((p `plusPtr` 44 :: Ptr QueryControlFlags)) (queryFlags)
    lift $ poke ((p `plusPtr` 48 :: Ptr QueryPipelineStatisticFlags)) (pipelineStatistics)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance PeekChain es => FromCStruct (CommandBufferInheritanceInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    renderPass <- peek @RenderPass ((p `plusPtr` 16 :: Ptr RenderPass))
    subpass <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    framebuffer <- peek @Framebuffer ((p `plusPtr` 32 :: Ptr Framebuffer))
    occlusionQueryEnable <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    queryFlags <- peek @QueryControlFlags ((p `plusPtr` 44 :: Ptr QueryControlFlags))
    pipelineStatistics <- peek @QueryPipelineStatisticFlags ((p `plusPtr` 48 :: Ptr QueryPipelineStatisticFlags))
    pure $ CommandBufferInheritanceInfo
             next renderPass subpass framebuffer (bool32ToBool occlusionQueryEnable) queryFlags pipelineStatistics

instance es ~ '[] => Zero (CommandBufferInheritanceInfo es) where
  zero = CommandBufferInheritanceInfo
           ()
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkCommandBufferBeginInfo - Structure specifying a command buffer begin
-- operation
--
-- == Valid Usage
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT',
--     the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pInheritanceInfo@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.RenderPass'
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT',
--     the @subpass@ member of @pInheritanceInfo@ /must/ be a valid subpass
--     index within the 'Graphics.Vulkan.Core10.Handles.RenderPass' member
--     of @pInheritanceInfo@
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT',
--     the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member of
--     @pInheritanceInfo@ /must/ be either
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', or a valid
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' that is compatible with
--     the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pInheritanceInfo@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupCommandBufferBeginInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.CommandBufferUsageFlagBits'
--     values
--
-- = See Also
--
-- 'CommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.CommandBufferUsageFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'beginCommandBuffer'
data CommandBufferBeginInfo (es :: [Type]) = CommandBufferBeginInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.CommandBufferUsageFlagBits'
    -- specifying usage behavior for the command buffer.
    flags :: CommandBufferUsageFlags
  , -- | @pInheritanceInfo@ is a pointer to a 'CommandBufferInheritanceInfo'
    -- structure, used if 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a
    -- secondary command buffer. If this is a primary command buffer, then this
    -- value is ignored.
    inheritanceInfo :: Maybe (SomeStruct CommandBufferInheritanceInfo)
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (CommandBufferBeginInfo es)

instance Extensible CommandBufferBeginInfo where
  extensibleType = STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  setNext x next = x{next = next}
  getNext CommandBufferBeginInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CommandBufferBeginInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeviceGroupCommandBufferBeginInfo = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (CommandBufferBeginInfo es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr CommandBufferUsageFlags)) (flags)
    pInheritanceInfo'' <- case (inheritanceInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (CommandBufferInheritanceInfo '[])) $ \cont -> withSomeCStruct @CommandBufferInheritanceInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (CommandBufferInheritanceInfo _)))) pInheritanceInfo''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance PeekChain es => FromCStruct (CommandBufferBeginInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @CommandBufferUsageFlags ((p `plusPtr` 16 :: Ptr CommandBufferUsageFlags))
    pInheritanceInfo <- peek @(Ptr (CommandBufferInheritanceInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (CommandBufferInheritanceInfo a))))
    pInheritanceInfo' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pInheritanceInfo
    pure $ CommandBufferBeginInfo
             next flags pInheritanceInfo'

instance es ~ '[] => Zero (CommandBufferBeginInfo es) where
  zero = CommandBufferBeginInfo
           ()
           zero
           Nothing

