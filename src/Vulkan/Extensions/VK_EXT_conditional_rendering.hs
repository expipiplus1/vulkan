{-# language CPP #-}
-- | = Name
--
-- VK_EXT_conditional_rendering - device extension
--
-- == VK_EXT_conditional_rendering
--
-- [__Name String__]
--     @VK_EXT_conditional_rendering@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     82
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_conditional_rendering] @vkushwaha%0A<<Here describe the issue or question you have about the VK_EXT_conditional_rendering extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-05-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Jesse Hall, Google
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Stuart Smith, Imagination Technologies
--
-- == Description
--
-- This extension allows the execution of one or more rendering commands to
-- be conditional on a value in buffer memory. This may help an application
-- reduce the latency by conditionally discarding rendering commands
-- without application intervention. The conditional rendering commands are
-- limited to draws, compute dispatches and clearing attachments within a
-- conditional rendering block.
--
-- == New Commands
--
-- -   'cmdBeginConditionalRenderingEXT'
--
-- -   'cmdEndConditionalRenderingEXT'
--
-- == New Structures
--
-- -   'ConditionalRenderingBeginInfoEXT'
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'CommandBufferInheritanceConditionalRenderingInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceConditionalRenderingFeaturesEXT'
--
-- == New Enums
--
-- -   'ConditionalRenderingFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ConditionalRenderingFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CONDITIONAL_RENDERING_EXTENSION_NAME'
--
-- -   'EXT_CONDITIONAL_RENDERING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should conditional rendering affect copy and blit commands?
--
-- __RESOLVED__: Conditional rendering should not affect copies and blits.
--
-- 2) Should secondary command buffers be allowed to execute while
-- conditional rendering is active in the primary command buffer?
--
-- __RESOLVED__: The rendering commands in secondary command buffer will be
-- affected by an active conditional rendering in primary command buffer if
-- the @conditionalRenderingEnable@ is set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'. Conditional rendering /must/ not
-- be active in the primary command buffer if @conditionalRenderingEnable@
-- is 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2018-04-19 (Vikram Kushwaha)
--
--     -   First Version
--
-- -   Revision 2, 2018-05-21 (Vikram Kushwaha)
--
--     -   Add new pipeline stage, access flags and limit conditional
--         rendering to a subpass or entire render pass.
--
-- == See Also
--
-- 'CommandBufferInheritanceConditionalRenderingInfoEXT',
-- 'ConditionalRenderingBeginInfoEXT', 'ConditionalRenderingFlagBitsEXT',
-- 'ConditionalRenderingFlagsEXT',
-- 'PhysicalDeviceConditionalRenderingFeaturesEXT',
-- 'cmdBeginConditionalRenderingEXT', 'cmdEndConditionalRenderingEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_conditional_rendering Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_conditional_rendering  ( cmdBeginConditionalRenderingEXT
                                                       , cmdUseConditionalRenderingEXT
                                                       , cmdEndConditionalRenderingEXT
                                                       , ConditionalRenderingBeginInfoEXT(..)
                                                       , CommandBufferInheritanceConditionalRenderingInfoEXT(..)
                                                       , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
                                                       , ConditionalRenderingFlagsEXT
                                                       , ConditionalRenderingFlagBitsEXT( CONDITIONAL_RENDERING_INVERTED_BIT_EXT
                                                                                        , ..
                                                                                        )
                                                       , EXT_CONDITIONAL_RENDERING_SPEC_VERSION
                                                       , pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION
                                                       , EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
                                                       , pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
                                                       ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginConditionalRenderingEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndConditionalRenderingEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginConditionalRenderingEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr ConditionalRenderingBeginInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr ConditionalRenderingBeginInfoEXT -> IO ()

-- | vkCmdBeginConditionalRenderingEXT - Define the beginning of a
-- conditional rendering block
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginConditionalRenderingEXT-None-01980# Conditional
--     rendering /must/ not already be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginConditionalRenderingEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginConditionalRenderingEXT-pConditionalRenderingBegin-parameter#
--     @pConditionalRenderingBegin@ /must/ be a valid pointer to a valid
--     'ConditionalRenderingBeginInfoEXT' structure
--
-- -   #VUID-vkCmdBeginConditionalRenderingEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginConditionalRenderingEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBeginConditionalRenderingEXT-videocoding# This command
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'ConditionalRenderingBeginInfoEXT'
cmdBeginConditionalRenderingEXT :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which this command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @pConditionalRenderingBegin@ is a pointer to a
                                   -- 'ConditionalRenderingBeginInfoEXT' structure specifying parameters of
                                   -- conditional rendering.
                                   ConditionalRenderingBeginInfoEXT
                                -> io ()
cmdBeginConditionalRenderingEXT commandBuffer conditionalRenderingBegin = liftIO . evalContT $ do
  let vkCmdBeginConditionalRenderingEXTPtr = pVkCmdBeginConditionalRenderingEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBeginConditionalRenderingEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginConditionalRenderingEXT is null" Nothing Nothing
  let vkCmdBeginConditionalRenderingEXT' = mkVkCmdBeginConditionalRenderingEXT vkCmdBeginConditionalRenderingEXTPtr
  pConditionalRenderingBegin <- ContT $ withCStruct (conditionalRenderingBegin)
  lift $ traceAroundEvent "vkCmdBeginConditionalRenderingEXT" (vkCmdBeginConditionalRenderingEXT' (commandBufferHandle (commandBuffer)) pConditionalRenderingBegin)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginConditionalRenderingEXT' and 'cmdEndConditionalRenderingEXT'
--
-- Note that 'cmdEndConditionalRenderingEXT' is *not* called if an
-- exception is thrown by the inner action.
cmdUseConditionalRenderingEXT :: forall io r . MonadIO io => CommandBuffer -> ConditionalRenderingBeginInfoEXT -> io r -> io r
cmdUseConditionalRenderingEXT commandBuffer pConditionalRenderingBegin a =
  (cmdBeginConditionalRenderingEXT commandBuffer pConditionalRenderingBegin) *> a <* (cmdEndConditionalRenderingEXT commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndConditionalRenderingEXT
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndConditionalRenderingEXT - Define the end of a conditional
-- rendering block
--
-- = Description
--
-- Once ended, conditional rendering becomes inactive.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-None-01985# Conditional
--     rendering /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-None-01986# If conditional
--     rendering was made
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#active-conditional-rendering active>
--     outside of a render pass instance, it /must/ not be ended inside a
--     render pass instance
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-None-01987# If conditional
--     rendering was made
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#active-conditional-rendering active>
--     within a subpass it /must/ be ended in the same subpass
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-videocoding# This command
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndConditionalRenderingEXT :: forall io
                               . (MonadIO io)
                              => -- | @commandBuffer@ is the command buffer into which this command will be
                                 -- recorded.
                                 CommandBuffer
                              -> io ()
cmdEndConditionalRenderingEXT commandBuffer = liftIO $ do
  let vkCmdEndConditionalRenderingEXTPtr = pVkCmdEndConditionalRenderingEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdEndConditionalRenderingEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndConditionalRenderingEXT is null" Nothing Nothing
  let vkCmdEndConditionalRenderingEXT' = mkVkCmdEndConditionalRenderingEXT vkCmdEndConditionalRenderingEXTPtr
  traceAroundEvent "vkCmdEndConditionalRenderingEXT" (vkCmdEndConditionalRenderingEXT' (commandBufferHandle (commandBuffer)))
  pure $ ()


-- | VkConditionalRenderingBeginInfoEXT - Structure specifying conditional
-- rendering begin information
--
-- = Description
--
-- If the 32-bit value at @offset@ in @buffer@ memory is zero, then the
-- rendering commands are discarded, otherwise they are executed as normal.
-- If the value of the predicate in buffer memory changes while conditional
-- rendering is active, the rendering commands /may/ be discarded in an
-- implementation-dependent way. Some implementations may latch the value
-- of the predicate upon beginning conditional rendering while others may
-- read it before every rendering command.
--
-- == Valid Usage
--
-- -   #VUID-VkConditionalRenderingBeginInfoEXT-buffer-01981# If @buffer@
--     is non-sparse then it /must/ be bound completely and contiguously to
--     a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkConditionalRenderingBeginInfoEXT-buffer-01982# @buffer@
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT'
--     bit set
--
-- -   #VUID-VkConditionalRenderingBeginInfoEXT-offset-01983# @offset@
--     /must/ be less than the size of @buffer@ by at least 32 bits
--
-- -   #VUID-VkConditionalRenderingBeginInfoEXT-offset-01984# @offset@
--     /must/ be a multiple of 4
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkConditionalRenderingBeginInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT'
--
-- -   #VUID-VkConditionalRenderingBeginInfoEXT-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkConditionalRenderingBeginInfoEXT-buffer-parameter# @buffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkConditionalRenderingBeginInfoEXT-flags-parameter# @flags@
--     /must/ be a valid combination of 'ConditionalRenderingFlagBitsEXT'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>,
-- 'Vulkan.Core10.Handles.Buffer', 'ConditionalRenderingFlagsEXT',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBeginConditionalRenderingEXT'
data ConditionalRenderingBeginInfoEXT = ConditionalRenderingBeginInfoEXT
  { -- | @buffer@ is a buffer containing the predicate for conditional rendering.
    buffer :: Buffer
  , -- | @offset@ is the byte offset into @buffer@ where the predicate is
    -- located.
    offset :: DeviceSize
  , -- | @flags@ is a bitmask of 'ConditionalRenderingFlagsEXT' specifying the
    -- behavior of conditional rendering.
    flags :: ConditionalRenderingFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ConditionalRenderingBeginInfoEXT)
#endif
deriving instance Show ConditionalRenderingBeginInfoEXT

instance ToCStruct ConditionalRenderingBeginInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ConditionalRenderingBeginInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 32 :: Ptr ConditionalRenderingFlagsEXT)) (flags)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct ConditionalRenderingBeginInfoEXT where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    flags <- peek @ConditionalRenderingFlagsEXT ((p `plusPtr` 32 :: Ptr ConditionalRenderingFlagsEXT))
    pure $ ConditionalRenderingBeginInfoEXT
             buffer offset flags

instance Storable ConditionalRenderingBeginInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ConditionalRenderingBeginInfoEXT where
  zero = ConditionalRenderingBeginInfoEXT
           zero
           zero
           zero


-- | VkCommandBufferInheritanceConditionalRenderingInfoEXT - Structure
-- specifying command buffer inheritance information
--
-- = Description
--
-- If this structure is not present, the behavior is as if
-- @conditionalRenderingEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage
--
-- -   #VUID-VkCommandBufferInheritanceConditionalRenderingInfoEXT-conditionalRenderingEnable-01977#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-inheritedConditionalRendering inheritedConditionalRendering>
--     feature is not enabled, @conditionalRenderingEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandBufferInheritanceConditionalRenderingInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data CommandBufferInheritanceConditionalRenderingInfoEXT = CommandBufferInheritanceConditionalRenderingInfoEXT
  { -- | @conditionalRenderingEnable@ specifies whether the command buffer /can/
    -- be executed while conditional rendering is active in the primary command
    -- buffer. If this is 'Vulkan.Core10.FundamentalTypes.TRUE', then this
    -- command buffer /can/ be executed whether the primary command buffer has
    -- active conditional rendering or not. If this is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', then the primary command buffer
    -- /must/ not have conditional rendering active.
    conditionalRenderingEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferInheritanceConditionalRenderingInfoEXT)
#endif
deriving instance Show CommandBufferInheritanceConditionalRenderingInfoEXT

instance ToCStruct CommandBufferInheritanceConditionalRenderingInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceConditionalRenderingInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (conditionalRenderingEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct CommandBufferInheritanceConditionalRenderingInfoEXT where
  peekCStruct p = do
    conditionalRenderingEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ CommandBufferInheritanceConditionalRenderingInfoEXT
             (bool32ToBool conditionalRenderingEnable)

instance Storable CommandBufferInheritanceConditionalRenderingInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandBufferInheritanceConditionalRenderingInfoEXT where
  zero = CommandBufferInheritanceConditionalRenderingInfoEXT
           zero


-- | VkPhysicalDeviceConditionalRenderingFeaturesEXT - Structure describing
-- if a secondary command buffer can be executed if conditional rendering
-- is active in the primary command buffer
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceConditionalRenderingFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceConditionalRenderingFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceConditionalRenderingFeaturesEXT = PhysicalDeviceConditionalRenderingFeaturesEXT
  { -- | #features-conditionalRendering# @conditionalRendering@ specifies whether
    -- conditional rendering is supported.
    conditionalRendering :: Bool
  , -- | #features-inheritedConditionalRendering# @inheritedConditionalRendering@
    -- specifies whether a secondary command buffer /can/ be executed while
    -- conditional rendering is active in the primary command buffer.
    inheritedConditionalRendering :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceConditionalRenderingFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceConditionalRenderingFeaturesEXT

instance ToCStruct PhysicalDeviceConditionalRenderingFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceConditionalRenderingFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (conditionalRendering))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (inheritedConditionalRendering))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceConditionalRenderingFeaturesEXT where
  peekCStruct p = do
    conditionalRendering <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    inheritedConditionalRendering <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceConditionalRenderingFeaturesEXT
             (bool32ToBool conditionalRendering) (bool32ToBool inheritedConditionalRendering)

instance Storable PhysicalDeviceConditionalRenderingFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceConditionalRenderingFeaturesEXT where
  zero = PhysicalDeviceConditionalRenderingFeaturesEXT
           zero
           zero


type ConditionalRenderingFlagsEXT = ConditionalRenderingFlagBitsEXT

-- | VkConditionalRenderingFlagBitsEXT - Specify the behavior of conditional
-- rendering
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>,
-- 'ConditionalRenderingFlagsEXT'
newtype ConditionalRenderingFlagBitsEXT = ConditionalRenderingFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'CONDITIONAL_RENDERING_INVERTED_BIT_EXT' specifies the condition used to
-- determine whether to discard rendering commands or not. That is, if the
-- 32-bit predicate read from @buffer@ memory at @offset@ is zero, the
-- rendering commands are not discarded, and if non zero, then they are
-- discarded.
pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT = ConditionalRenderingFlagBitsEXT 0x00000001

conNameConditionalRenderingFlagBitsEXT :: String
conNameConditionalRenderingFlagBitsEXT = "ConditionalRenderingFlagBitsEXT"

enumPrefixConditionalRenderingFlagBitsEXT :: String
enumPrefixConditionalRenderingFlagBitsEXT = "CONDITIONAL_RENDERING_INVERTED_BIT_EXT"

showTableConditionalRenderingFlagBitsEXT :: [(ConditionalRenderingFlagBitsEXT, String)]
showTableConditionalRenderingFlagBitsEXT = [(CONDITIONAL_RENDERING_INVERTED_BIT_EXT, "")]

instance Show ConditionalRenderingFlagBitsEXT where
  showsPrec = enumShowsPrec enumPrefixConditionalRenderingFlagBitsEXT
                            showTableConditionalRenderingFlagBitsEXT
                            conNameConditionalRenderingFlagBitsEXT
                            (\(ConditionalRenderingFlagBitsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ConditionalRenderingFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixConditionalRenderingFlagBitsEXT
                          showTableConditionalRenderingFlagBitsEXT
                          conNameConditionalRenderingFlagBitsEXT
                          ConditionalRenderingFlagBitsEXT


type EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION"
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2


type EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME"
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"

