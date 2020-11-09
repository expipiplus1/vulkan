{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_conditional_rendering  ( cmdBeginConditionalRenderingEXT
                                                       , cmdUseConditionalRenderingEXT
                                                       , cmdEndConditionalRenderingEXT
                                                       , ConditionalRenderingBeginInfoEXT(..)
                                                       , CommandBufferInheritanceConditionalRenderingInfoEXT(..)
                                                       , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
                                                       , ConditionalRenderingFlagBitsEXT( CONDITIONAL_RENDERING_INVERTED_BIT_EXT
                                                                                        , ..
                                                                                        )
                                                       , ConditionalRenderingFlagsEXT
                                                       , EXT_CONDITIONAL_RENDERING_SPEC_VERSION
                                                       , pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION
                                                       , EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
                                                       , pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
                                                       ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
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
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginConditionalRenderingEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndConditionalRenderingEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
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
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
  let vkCmdBeginConditionalRenderingEXTPtr = pVkCmdBeginConditionalRenderingEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginConditionalRenderingEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginConditionalRenderingEXT is null" Nothing Nothing
  let vkCmdBeginConditionalRenderingEXT' = mkVkCmdBeginConditionalRenderingEXT vkCmdBeginConditionalRenderingEXTPtr
  pConditionalRenderingBegin <- ContT $ withCStruct (conditionalRenderingBegin)
  lift $ vkCmdBeginConditionalRenderingEXT' (commandBufferHandle (commandBuffer)) pConditionalRenderingBegin
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-None-01986# If conditional
--     rendering was made
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
--     outside of a render pass instance, it /must/ not be ended inside a
--     render pass instance
--
-- -   #VUID-vkCmdEndConditionalRenderingEXT-None-01987# If conditional
--     rendering was made
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
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
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndConditionalRenderingEXT :: forall io
                               . (MonadIO io)
                              => -- | @commandBuffer@ is the command buffer into which this command will be
                                 -- recorded.
                                 CommandBuffer
                              -> io ()
cmdEndConditionalRenderingEXT commandBuffer = liftIO $ do
  let vkCmdEndConditionalRenderingEXTPtr = pVkCmdEndConditionalRenderingEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndConditionalRenderingEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndConditionalRenderingEXT is null" Nothing Nothing
  let vkCmdEndConditionalRenderingEXT' = mkVkCmdEndConditionalRenderingEXT vkCmdEndConditionalRenderingEXTPtr
  vkCmdEndConditionalRenderingEXT' (commandBufferHandle (commandBuffer))
  pure $ ()


-- | VkConditionalRenderingBeginInfoEXT - Structure specifying conditional
-- rendering begin info
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
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
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
-- specifying command buffer inheritance info
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inheritedConditionalRendering inherited conditional rendering>
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #features-conditionalRendering# @conditionalRendering@ specifies
--     whether conditional rendering is supported.
--
-- -   #features-inheritedConditionalRendering#
--     @inheritedConditionalRendering@ specifies whether a secondary
--     command buffer /can/ be executed while conditional rendering is
--     active in the primary command buffer.
--
-- = Description
--
-- If the 'PhysicalDeviceConditionalRenderingFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating the implementation-dependent
-- behavior. 'PhysicalDeviceConditionalRenderingFeaturesEXT' /can/ also be
-- included in @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable the features.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceConditionalRenderingFeaturesEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceConditionalRenderingFeaturesEXT = PhysicalDeviceConditionalRenderingFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceConditionalRenderingFeaturesEXT" "conditionalRendering"
    conditionalRendering :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceConditionalRenderingFeaturesEXT" "inheritedConditionalRendering"
    inheritedConditionalRendering :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceConditionalRenderingFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceConditionalRenderingFeaturesEXT

instance ToCStruct PhysicalDeviceConditionalRenderingFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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


-- | VkConditionalRenderingFlagBitsEXT - Specify the behavior of conditional
-- rendering
--
-- = See Also
--
-- 'ConditionalRenderingFlagsEXT'
newtype ConditionalRenderingFlagBitsEXT = ConditionalRenderingFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'CONDITIONAL_RENDERING_INVERTED_BIT_EXT' specifies the condition used to
-- determine whether to discard rendering commands or not. That is, if the
-- 32-bit predicate read from @buffer@ memory at @offset@ is zero, the
-- rendering commands are not discarded, and if non zero, then they are
-- discarded.
pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT = ConditionalRenderingFlagBitsEXT 0x00000001

type ConditionalRenderingFlagsEXT = ConditionalRenderingFlagBitsEXT

instance Show ConditionalRenderingFlagBitsEXT where
  showsPrec p = \case
    CONDITIONAL_RENDERING_INVERTED_BIT_EXT -> showString "CONDITIONAL_RENDERING_INVERTED_BIT_EXT"
    ConditionalRenderingFlagBitsEXT x -> showParen (p >= 11) (showString "ConditionalRenderingFlagBitsEXT 0x" . showHex x)

instance Read ConditionalRenderingFlagBitsEXT where
  readPrec = parens (choose [("CONDITIONAL_RENDERING_INVERTED_BIT_EXT", pure CONDITIONAL_RENDERING_INVERTED_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ConditionalRenderingFlagBitsEXT")
                       v <- step readPrec
                       pure (ConditionalRenderingFlagBitsEXT v)))


type EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION"
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2


type EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME"
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"

