{-# language CPP #-}
-- | = Name
--
-- VK_EXT_extended_dynamic_state2 - device extension
--
-- == VK_EXT_extended_dynamic_state2
--
-- [__Name String__]
--     @VK_EXT_extended_dynamic_state2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     378
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_extended_dynamic_state2] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_EXT_extended_dynamic_state2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-04-12
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds some more dynamic state to support applications that
-- need to reduce the number of pipeline state objects they compile and
-- bind.
--
-- == New Commands
--
-- -   'cmdSetDepthBiasEnableEXT'
--
-- -   'cmdSetLogicOpEXT'
--
-- -   'cmdSetPatchControlPointsEXT'
--
-- -   'cmdSetPrimitiveRestartEnableEXT'
--
-- -   'cmdSetRasterizerDiscardEnableEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExtendedDynamicState2FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_2_EXTENSION_NAME'
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'DYNAMIC_STATE_DEPTH_BIAS_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--
--     -   'DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT'
--
--     -   'DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_2_FEATURES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- This extension has been partially promoted. The dynamic state enumerants
-- 'DYNAMIC_STATE_DEPTH_BIAS_ENABLE_EXT',
-- 'DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT', and
-- 'DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE_EXT'; and the corresponding
-- entry points in this extension are included in core Vulkan 1.3, with the
-- EXT suffix omitted. The enumerants and entry points for dynamic logic
-- operation and patch control points are not promoted, nor is the feature
-- structure. Extension interfaces that were promoted remain available as
-- aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2021-04-12 (Vikram Kushwaha)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceExtendedDynamicState2FeaturesEXT',
-- 'cmdSetDepthBiasEnableEXT', 'cmdSetLogicOpEXT',
-- 'cmdSetPatchControlPointsEXT', 'cmdSetPrimitiveRestartEnableEXT',
-- 'cmdSetRasterizerDiscardEnableEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_extended_dynamic_state2  ( cmdSetPatchControlPointsEXT
                                                         , cmdSetLogicOpEXT
                                                         , pattern DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE_EXT
                                                         , pattern DYNAMIC_STATE_DEPTH_BIAS_ENABLE_EXT
                                                         , pattern DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT
                                                         , cmdSetRasterizerDiscardEnableEXT
                                                         , cmdSetDepthBiasEnableEXT
                                                         , cmdSetPrimitiveRestartEnableEXT
                                                         , PhysicalDeviceExtendedDynamicState2FeaturesEXT(..)
                                                         , EXT_EXTENDED_DYNAMIC_STATE_2_SPEC_VERSION
                                                         , pattern EXT_EXTENDED_DYNAMIC_STATE_2_SPEC_VERSION
                                                         , EXT_EXTENDED_DYNAMIC_STATE_2_EXTENSION_NAME
                                                         , pattern EXT_EXTENDED_DYNAMIC_STATE_2_EXTENSION_NAME
                                                         ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2 (cmdSetDepthBiasEnable)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2 (cmdSetPrimitiveRestartEnable)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2 (cmdSetRasterizerDiscardEnable)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLogicOpEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPatchControlPointsEXT))
import Vulkan.Core10.Enums.LogicOp (LogicOp)
import Vulkan.Core10.Enums.LogicOp (LogicOp(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_DEPTH_BIAS_ENABLE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_2_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPatchControlPointsEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> IO ()

-- | vkCmdSetPatchControlPointsEXT - Specify the number of control points per
-- patch dynamically for a command buffer
--
-- = Description
--
-- This command sets the number of control points per patch for subsequent
-- drawing commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo'::@patchControlPoints@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetPatchControlPointsEXT-None-04873# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedDynamicState2PatchControlPoints extendedDynamicState2PatchControlPoints>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetPatchControlPointsEXT-patchControlPoints-04874#
--     @patchControlPoints@ /must/ be greater than zero and less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetPatchControlPointsEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetPatchControlPointsEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetPatchControlPointsEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetPatchControlPointsEXT-videocoding# This command /must/
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state2 VK_EXT_extended_dynamic_state2>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetPatchControlPointsEXT :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command will be
                               -- recorded.
                               CommandBuffer
                            -> -- | @patchControlPoints@ specifies the number of control points per patch.
                               ("patchControlPoints" ::: Word32)
                            -> io ()
cmdSetPatchControlPointsEXT commandBuffer patchControlPoints = liftIO $ do
  let vkCmdSetPatchControlPointsEXTPtr = pVkCmdSetPatchControlPointsEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetPatchControlPointsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPatchControlPointsEXT is null" Nothing Nothing
  let vkCmdSetPatchControlPointsEXT' = mkVkCmdSetPatchControlPointsEXT vkCmdSetPatchControlPointsEXTPtr
  traceAroundEvent "vkCmdSetPatchControlPointsEXT" (vkCmdSetPatchControlPointsEXT' (commandBufferHandle (commandBuffer)) (patchControlPoints))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLogicOpEXT
  :: FunPtr (Ptr CommandBuffer_T -> LogicOp -> IO ()) -> Ptr CommandBuffer_T -> LogicOp -> IO ()

-- | vkCmdSetLogicOpEXT - Select which logical operation to apply for blend
-- state dynamically for a command buffer
--
-- = Description
--
-- This command sets the logical operation for blend state for subsequent
-- drawing commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@logicOp@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLogicOpEXT-None-04867# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedDynamicState2LogicOp extendedDynamicState2LogicOp>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetLogicOpEXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetLogicOpEXT-logicOp-parameter# @logicOp@ /must/ be a
--     valid 'Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- -   #VUID-vkCmdSetLogicOpEXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetLogicOpEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetLogicOpEXT-videocoding# This command /must/ only be
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state2 VK_EXT_extended_dynamic_state2>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.LogicOp.LogicOp'
cmdSetLogicOpEXT :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is the command buffer into which the command will be
                    -- recorded.
                    CommandBuffer
                 -> -- | @logicOp@ specifies the logical operation to apply for blend state.
                    LogicOp
                 -> io ()
cmdSetLogicOpEXT commandBuffer logicOp = liftIO $ do
  let vkCmdSetLogicOpEXTPtr = pVkCmdSetLogicOpEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetLogicOpEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLogicOpEXT is null" Nothing Nothing
  let vkCmdSetLogicOpEXT' = mkVkCmdSetLogicOpEXT vkCmdSetLogicOpEXTPtr
  traceAroundEvent "vkCmdSetLogicOpEXT" (vkCmdSetLogicOpEXT' (commandBufferHandle (commandBuffer)) (logicOp))
  pure $ ()


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE_EXT"
pattern DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE_EXT = DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_DEPTH_BIAS_ENABLE_EXT"
pattern DYNAMIC_STATE_DEPTH_BIAS_ENABLE_EXT = DYNAMIC_STATE_DEPTH_BIAS_ENABLE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT"
pattern DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT = DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE


-- No documentation found for TopLevel "vkCmdSetRasterizerDiscardEnableEXT"
cmdSetRasterizerDiscardEnableEXT = cmdSetRasterizerDiscardEnable


-- No documentation found for TopLevel "vkCmdSetDepthBiasEnableEXT"
cmdSetDepthBiasEnableEXT = cmdSetDepthBiasEnable


-- No documentation found for TopLevel "vkCmdSetPrimitiveRestartEnableEXT"
cmdSetPrimitiveRestartEnableEXT = cmdSetPrimitiveRestartEnable


-- | VkPhysicalDeviceExtendedDynamicState2FeaturesEXT - Structure describing
-- what extended dynamic state can be used
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceExtendedDynamicState2FeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceExtendedDynamicState2FeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state2 VK_EXT_extended_dynamic_state2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExtendedDynamicState2FeaturesEXT = PhysicalDeviceExtendedDynamicState2FeaturesEXT
  { -- | #features-extendedDynamicState2# @extendedDynamicState2@ indicates that
    -- the implementation supports the following dynamic states:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
    extendedDynamicState2 :: Bool
  , -- | #features-extendedDynamicState2LogicOp# @extendedDynamicState2LogicOp@
    -- indicates that the implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
    extendedDynamicState2LogicOp :: Bool
  , -- | #features-extendedDynamicState2PatchControlPoints#
    -- @extendedDynamicState2PatchControlPoints@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
    extendedDynamicState2PatchControlPoints :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExtendedDynamicState2FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceExtendedDynamicState2FeaturesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicState2FeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExtendedDynamicState2FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState2))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState2LogicOp))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState2PatchControlPoints))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExtendedDynamicState2FeaturesEXT where
  peekCStruct p = do
    extendedDynamicState2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    extendedDynamicState2LogicOp <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    extendedDynamicState2PatchControlPoints <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceExtendedDynamicState2FeaturesEXT
             (bool32ToBool extendedDynamicState2) (bool32ToBool extendedDynamicState2LogicOp) (bool32ToBool extendedDynamicState2PatchControlPoints)

instance Storable PhysicalDeviceExtendedDynamicState2FeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExtendedDynamicState2FeaturesEXT where
  zero = PhysicalDeviceExtendedDynamicState2FeaturesEXT
           zero
           zero
           zero


type EXT_EXTENDED_DYNAMIC_STATE_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_2_SPEC_VERSION"
pattern EXT_EXTENDED_DYNAMIC_STATE_2_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTENDED_DYNAMIC_STATE_2_SPEC_VERSION = 1


type EXT_EXTENDED_DYNAMIC_STATE_2_EXTENSION_NAME = "VK_EXT_extended_dynamic_state2"

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_2_EXTENSION_NAME"
pattern EXT_EXTENDED_DYNAMIC_STATE_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTENDED_DYNAMIC_STATE_2_EXTENSION_NAME = "VK_EXT_extended_dynamic_state2"

