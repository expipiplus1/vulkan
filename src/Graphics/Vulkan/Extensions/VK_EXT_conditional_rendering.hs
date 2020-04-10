{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering  ( cmdBeginConditionalRenderingEXT
                                                                , cmdWithConditionalRenderingEXT
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

import Control.Exception.Base (bracket_)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBeginConditionalRenderingEXT))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdEndConditionalRenderingEXT))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginConditionalRenderingEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr ConditionalRenderingBeginInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr ConditionalRenderingBeginInfoEXT -> IO ()

-- | vkCmdBeginConditionalRenderingEXT - Define the beginning of a
-- conditional rendering block
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @pConditionalRenderingBegin@ is a pointer to a
--     'ConditionalRenderingBeginInfoEXT' structure specifying parameters
--     of conditional rendering.
--
-- == Valid Usage
--
-- -   Conditional rendering /must/ not already be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pConditionalRenderingBegin@ /must/ be a valid pointer to a valid
--     'ConditionalRenderingBeginInfoEXT' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
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
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'ConditionalRenderingBeginInfoEXT'
cmdBeginConditionalRenderingEXT :: CommandBuffer -> ConditionalRenderingBeginInfoEXT -> IO ()
cmdBeginConditionalRenderingEXT commandBuffer conditionalRenderingBegin = evalContT $ do
  let vkCmdBeginConditionalRenderingEXT' = mkVkCmdBeginConditionalRenderingEXT (pVkCmdBeginConditionalRenderingEXT (deviceCmds (commandBuffer :: CommandBuffer)))
  pConditionalRenderingBegin <- ContT $ withCStruct (conditionalRenderingBegin)
  lift $ vkCmdBeginConditionalRenderingEXT' (commandBufferHandle (commandBuffer)) pConditionalRenderingBegin
  pure $ ()

-- | A safe wrapper for 'cmdBeginConditionalRenderingEXT' and
-- 'cmdEndConditionalRenderingEXT' using 'bracket_'
cmdWithConditionalRenderingEXT :: CommandBuffer -> ConditionalRenderingBeginInfoEXT -> IO r -> IO r
cmdWithConditionalRenderingEXT commandBuffer pConditionalRenderingBegin =
  bracket_
    (cmdBeginConditionalRenderingEXT commandBuffer pConditionalRenderingBegin)
    (cmdEndConditionalRenderingEXT commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndConditionalRenderingEXT
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndConditionalRenderingEXT - Define the end of a conditional
-- rendering block
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- = Description
--
-- Once ended, conditional rendering becomes inactive.
--
-- == Valid Usage
--
-- -   Conditional rendering /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- -   If conditional rendering was made
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
--     outside of a render pass instance, it must not be ended inside a
--     render pass instance
--
-- -   If conditional rendering was made
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#active-conditional-rendering active>
--     within a subpass it must be ended in the same subpass
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
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
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdEndConditionalRenderingEXT :: CommandBuffer -> IO ()
cmdEndConditionalRenderingEXT commandBuffer = do
  let vkCmdEndConditionalRenderingEXT' = mkVkCmdEndConditionalRenderingEXT (pVkCmdEndConditionalRenderingEXT (deviceCmds (commandBuffer :: CommandBuffer)))
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
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT'
--     bit set
--
-- -   @offset@ /must/ be less than the size of @buffer@ by at least 32
--     bits.
--
-- -   @offset@ /must/ be a multiple of 4
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @buffer@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   @flags@ /must/ be a valid combination of
--     'ConditionalRenderingFlagBitsEXT' values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer', 'ConditionalRenderingFlagsEXT',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
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
  deriving (Typeable)
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
-- @conditionalRenderingEnable@ is 'Graphics.Vulkan.Core10.BaseType.FALSE'.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inheritedConditionalRendering inherited conditional rendering>
--     feature is not enabled, @conditionalRenderingEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data CommandBufferInheritanceConditionalRenderingInfoEXT = CommandBufferInheritanceConditionalRenderingInfoEXT
  { -- | @conditionalRenderingEnable@ specifies whether the command buffer /can/
    -- be executed while conditional rendering is active in the primary command
    -- buffer. If this is 'Graphics.Vulkan.Core10.BaseType.TRUE', then this
    -- command buffer /can/ be executed whether the primary command buffer has
    -- active conditional rendering or not. If this is
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE', then the primary command buffer
    -- /must/ not have conditional rendering active.
    conditionalRenderingEnable :: Bool }
  deriving (Typeable)
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
-- = Description
--
-- If the 'PhysicalDeviceConditionalRenderingFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating the implementation-dependent
-- behavior. 'PhysicalDeviceConditionalRenderingFeaturesEXT' /can/ also be
-- included in @pNext@ chain of
-- 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to enable the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceConditionalRenderingFeaturesEXT = PhysicalDeviceConditionalRenderingFeaturesEXT
  { -- | @conditionalRendering@ specifies whether conditional rendering is
    -- supported.
    conditionalRendering :: Bool
  , -- | @inheritedConditionalRendering@ specifies whether a secondary command
    -- buffer /can/ be executed while conditional rendering is active in the
    -- primary command buffer.
    inheritedConditionalRendering :: Bool
  }
  deriving (Typeable)
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

