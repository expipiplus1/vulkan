{-# language CPP #-}
-- | = Name
--
-- VK_EXT_attachment_feedback_loop_dynamic_state - device extension
--
-- == VK_EXT_attachment_feedback_loop_dynamic_state
--
-- [__Name String__]
--     @VK_EXT_attachment_feedback_loop_dynamic_state@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     525
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_attachment_feedback_loop_dynamic_state] @zmike%0A*Here describe the issue or question you have about the VK_EXT_attachment_feedback_loop_dynamic_state extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_attachment_feedback_loop_dynamic_state.adoc VK_EXT_attachment_feedback_loop_dynamic_state>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-04-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Daniel Story, Nintendo
--
--     -   Stu Smith, AMD
--
--     -   Samuel Pitoiset, Valve
--
--     -   Ricardo Garcia, Igalia
--
-- == Description
--
-- This extension adds support for setting attachment feedback loops
-- dynamically on command buffers.
--
-- == New Commands
--
-- -   'cmdSetAttachmentFeedbackLoopEnableEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_EXTENSION_NAME'
--
-- -   'EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2023-04-28 (Mike Blumenkrantz)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT',
-- 'cmdSetAttachmentFeedbackLoopEnableEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_dynamic_state Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state  ( cmdSetAttachmentFeedbackLoopEnableEXT
                                                                        , PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT(..)
                                                                        , EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_SPEC_VERSION
                                                                        , pattern EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_SPEC_VERSION
                                                                        , EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_EXTENSION_NAME
                                                                        , pattern EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_EXTENSION_NAME
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetAttachmentFeedbackLoopEnableEXT))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetAttachmentFeedbackLoopEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> ImageAspectFlags -> IO ()) -> Ptr CommandBuffer_T -> ImageAspectFlags -> IO ()

-- | vkCmdSetAttachmentFeedbackLoopEnableEXT - Specify whether attachment
-- feedback loops are enabled dynamically on a command buffer
--
-- = Description
--
-- For attachments that are written to in a render pass, only attachments
-- with the aspects specified in @aspectMask@ /can/ be accessed as
-- non-attachments by subsequent
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#drawing drawing commands>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetAttachmentFeedbackLoopEnableEXT-attachmentFeedbackLoopDynamicState-08862#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-attachmentFeedbackLoopDynamicState attachmentFeedbackLoopDynamicState>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetAttachmentFeedbackLoopEnableEXT-aspectMask-08863#
--     @aspectMask@ /must/ only include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_NONE',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkCmdSetAttachmentFeedbackLoopEnableEXT-attachmentFeedbackLoopLayout-08864#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-attachmentFeedbackLoopLayout attachmentFeedbackLoopLayout>
--     feature is not enabled, @aspectMask@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_NONE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetAttachmentFeedbackLoopEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetAttachmentFeedbackLoopEnableEXT-aspectMask-parameter#
--     @aspectMask@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' values
--
-- -   #VUID-vkCmdSetAttachmentFeedbackLoopEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetAttachmentFeedbackLoopEnableEXT-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetAttachmentFeedbackLoopEnableEXT-videocoding# This
--     command /must/ only be called outside of a video coding scope
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_dynamic_state VK_EXT_attachment_feedback_loop_dynamic_state>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags'
cmdSetAttachmentFeedbackLoopEnableEXT :: forall io
                                       . (MonadIO io)
                                      => -- | @commandBuffer@ is the command buffer into which the command will be
                                         -- recorded.
                                         CommandBuffer
                                      -> -- | @aspectMask@ specifies the types of attachments for which feedback loops
                                         -- will be enabled. Attachment types whose aspects are not included in
                                         -- @aspectMask@ will have feedback loops disabled.
                                         ("aspectMask" ::: ImageAspectFlags)
                                      -> io ()
cmdSetAttachmentFeedbackLoopEnableEXT commandBuffer aspectMask = liftIO $ do
  let vkCmdSetAttachmentFeedbackLoopEnableEXTPtr = pVkCmdSetAttachmentFeedbackLoopEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetAttachmentFeedbackLoopEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetAttachmentFeedbackLoopEnableEXT is null" Nothing Nothing
  let vkCmdSetAttachmentFeedbackLoopEnableEXT' = mkVkCmdSetAttachmentFeedbackLoopEnableEXT vkCmdSetAttachmentFeedbackLoopEnableEXTPtr
  traceAroundEvent "vkCmdSetAttachmentFeedbackLoopEnableEXT" (vkCmdSetAttachmentFeedbackLoopEnableEXT'
                                                                (commandBufferHandle (commandBuffer))
                                                                (aspectMask))
  pure $ ()


-- | VkPhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT -
-- Structure describing if dynamic feedback loops can be used
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_dynamic_state VK_EXT_attachment_feedback_loop_dynamic_state>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT = PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT
  { -- | #features-attachmentFeedbackLoopDynamicState#
    -- @attachmentFeedbackLoopDynamicState@ specifies whether dynamic feedback
    -- loops are supported.
    attachmentFeedbackLoopDynamicState :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT

instance ToCStruct PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (attachmentFeedbackLoopDynamicState))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT where
  peekCStruct p = do
    attachmentFeedbackLoopDynamicState <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT
             (bool32ToBool attachmentFeedbackLoopDynamicState)

instance Storable PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT where
  zero = PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT
           zero


type EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_SPEC_VERSION"
pattern EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_SPEC_VERSION = 1


type EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_attachment_feedback_loop_dynamic_state"

-- No documentation found for TopLevel "VK_EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_EXTENSION_NAME"
pattern EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_attachment_feedback_loop_dynamic_state"

