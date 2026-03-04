{-# language CPP #-}
-- | = Name
--
-- VK_KHR_dynamic_rendering_local_read - device extension
--
-- == VK_KHR_dynamic_rendering_local_read
--
-- [__Name String__]
--     @VK_KHR_dynamic_rendering_local_read@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     233
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_dynamic_rendering_local_read] @tobski%0A*Here describe the issue or question you have about the VK_KHR_dynamic_rendering_local_read extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_dynamic_rendering_local_read.adoc VK_KHR_dynamic_rendering_local_read>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-11-03
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Connor Abbott, Valve
--
--     -   Pan Gao, Huawei
--
--     -   Lionel Landwerlin, Intel
--
--     -   Shahbaz Youssefi, Google
--
--     -   Alyssa Rosenzweig, Valve
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Graeme Leese, Broadcom
--
--     -   Piers Daniell, Nvidia
--
--     -   Stuart Smith, AMD
--
--     -   Daniel Story, Nintendo
--
--     -   James Fitzpatrick, Imagination
--
--     -   Piotr Byszewski, Mobica
--
--     -   Spencer Fricke, LunarG
--
--     -   Tom Olson, Arm
--
--     -   Michal Pietrasiuk, Intel
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Marty Johnson, Khronos
--
--     -   Wyvern Wang, Huawei
--
--     -   Jeff Bolz, Nvidia
--
--     -   Samuel (Sheng-Wen) Huang, MediaTek
--
-- == Description
--
-- This extension enables reads from attachments and resources written by
-- previous fragment shaders within a dynamic render pass.
--
-- == New Commands
--
-- -   'cmdSetRenderingAttachmentLocationsKHR'
--
-- -   'cmdSetRenderingInputAttachmentIndicesKHR'
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'RenderingAttachmentLocationInfoKHR'
--
--     -   'RenderingInputAttachmentIndexInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME'
--
-- -   'KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_RENDERING_LOCAL_READ_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2023-11-03 (Tobias Hector)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR',
-- 'RenderingAttachmentLocationInfoKHR',
-- 'RenderingInputAttachmentIndexInfoKHR',
-- 'cmdSetRenderingAttachmentLocationsKHR',
-- 'cmdSetRenderingInputAttachmentIndicesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read  ( cmdSetRenderingAttachmentLocationsKHR
                                                              , cmdSetRenderingInputAttachmentIndicesKHR
                                                              , PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR(..)
                                                              , RenderingAttachmentLocationInfoKHR(..)
                                                              , RenderingInputAttachmentIndexInfoKHR(..)
                                                              , KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION
                                                              , pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION
                                                              , KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME
                                                              , pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME
                                                              ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Marshal.Utils (with)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
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
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRenderingAttachmentLocationsKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRenderingInputAttachmentIndicesKHR))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRenderingAttachmentLocationsKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr RenderingAttachmentLocationInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr RenderingAttachmentLocationInfoKHR -> IO ()

-- | vkCmdSetRenderingAttachmentLocationsKHR - Set color attachment location
-- mappings for a command buffer
--
-- = Description
--
-- This command sets the attachment location mappings for subsequent
-- drawing commands, and /must/ match the mappings provided to the
-- currently bound pipeline, if one is bound, which /can/ be set by
-- chaining 'RenderingAttachmentLocationInfoKHR' to
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'.
--
-- Until this command is called, mappings in the command buffer state are
-- treated as each color attachment specified in
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
-- having a location equal to its index in
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@.
-- This state is reset whenever
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
-- is called.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-dynamicRenderingLocalRead-09509#
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     /must/ be enabled
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-pLocationInfo-09510#
--     @pLocationInfo->colorAttachmentCount@ /must/ be equal to the value
--     of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     used to begin the current render pass instance
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-commandBuffer-09511#
--     The current render pass instance /must/ have been started or resumed
--     by
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     in this @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-pLocationInfo-parameter#
--     @pLocationInfo@ /must/ be a valid pointer to a valid
--     'RenderingAttachmentLocationInfoKHR' structure
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-renderpass# This
--     command /must/ only be called inside of a render pass instance
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocationsKHR-videocoding# This
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'RenderingAttachmentLocationInfoKHR'
cmdSetRenderingAttachmentLocationsKHR :: forall io
                                       . (MonadIO io)
                                      => -- | @commandBuffer@ is the command buffer into which the command will be
                                         -- recorded.
                                         CommandBuffer
                                      -> -- | @pLocationInfo@ is a 'RenderingAttachmentLocationInfoKHR' structure
                                         -- indicating the new mappings.
                                         RenderingAttachmentLocationInfoKHR
                                      -> io ()
cmdSetRenderingAttachmentLocationsKHR commandBuffer
                                        locationInfo = liftIO . evalContT $ do
  let vkCmdSetRenderingAttachmentLocationsKHRPtr = pVkCmdSetRenderingAttachmentLocationsKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetRenderingAttachmentLocationsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRenderingAttachmentLocationsKHR is null" Nothing Nothing
  let vkCmdSetRenderingAttachmentLocationsKHR' = mkVkCmdSetRenderingAttachmentLocationsKHR vkCmdSetRenderingAttachmentLocationsKHRPtr
  pLocationInfo <- ContT $ withCStruct (locationInfo)
  lift $ traceAroundEvent "vkCmdSetRenderingAttachmentLocationsKHR" (vkCmdSetRenderingAttachmentLocationsKHR'
                                                                       (commandBufferHandle (commandBuffer))
                                                                       pLocationInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRenderingInputAttachmentIndicesKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr RenderingInputAttachmentIndexInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr RenderingInputAttachmentIndexInfoKHR -> IO ()

-- | vkCmdSetRenderingInputAttachmentIndicesKHR - Set input attachment index
-- mappings for a command buffer
--
-- = Description
--
-- This command sets the input attachment index mappings for subsequent
-- drawing commands, and /must/ match the mappings provided to the
-- currently bound pipeline, if one is bound, which /can/ be set by
-- chaining 'RenderingInputAttachmentIndexInfoKHR' to
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'.
--
-- Until this command is called, mappings in the command buffer state are
-- treated as each color attachment specified in
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
-- mapping to subpass inputs with a @InputAttachmentIndex@ equal to its
-- index in
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@,
-- and depth\/stencil attachments mapping to input attachments without
-- these decorations. This state is reset whenever
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
-- is called.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-dynamicRenderingLocalRead-09516#
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     /must/ be enabled
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-pInputAttachmentIndexInfo-09517#
--     @pInputAttachmentIndexInfo->colorAttachmentCount@ /must/ be equal to
--     the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     used to begin the current render pass instance
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-commandBuffer-09518#
--     The current render pass instance /must/ have been started or resumed
--     by
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     in this @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-pLocationInfo-parameter#
--     @pLocationInfo@ /must/ be a valid pointer to a valid
--     'RenderingInputAttachmentIndexInfoKHR' structure
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-renderpass# This
--     command /must/ only be called inside of a render pass instance
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndicesKHR-videocoding# This
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'RenderingInputAttachmentIndexInfoKHR'
cmdSetRenderingInputAttachmentIndicesKHR :: forall io
                                          . (MonadIO io)
                                         => -- | @commandBuffer@ is the command buffer into which the command will be
                                            -- recorded.
                                            CommandBuffer
                                         -> -- No documentation found for Nested "vkCmdSetRenderingInputAttachmentIndicesKHR" "pLocationInfo"
                                            ("locationInfo" ::: RenderingInputAttachmentIndexInfoKHR)
                                         -> io ()
cmdSetRenderingInputAttachmentIndicesKHR commandBuffer
                                           locationInfo = liftIO . evalContT $ do
  let vkCmdSetRenderingInputAttachmentIndicesKHRPtr = pVkCmdSetRenderingInputAttachmentIndicesKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetRenderingInputAttachmentIndicesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRenderingInputAttachmentIndicesKHR is null" Nothing Nothing
  let vkCmdSetRenderingInputAttachmentIndicesKHR' = mkVkCmdSetRenderingInputAttachmentIndicesKHR vkCmdSetRenderingInputAttachmentIndicesKHRPtr
  pLocationInfo <- ContT $ withCStruct (locationInfo)
  lift $ traceAroundEvent "vkCmdSetRenderingInputAttachmentIndicesKHR" (vkCmdSetRenderingInputAttachmentIndicesKHR'
                                                                          (commandBufferHandle (commandBuffer))
                                                                          pLocationInfo)
  pure $ ()


-- | VkPhysicalDeviceDynamicRenderingLocalReadFeaturesKHR - Structure
-- indicating support for local reads in dynamic render pass instances
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR = PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR
  { -- | #features-dynamicRenderingLocalRead# @dynamicRenderingLocalRead@
    -- specifies that the implementation supports local reads inside dynamic
    -- render pass instances using the
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
    -- command.
    dynamicRenderingLocalRead :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR

instance ToCStruct PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dynamicRenderingLocalRead))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR where
  peekCStruct p = do
    dynamicRenderingLocalRead <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR
             (bool32ToBool dynamicRenderingLocalRead)

instance Storable PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR where
  zero = PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR
           zero


-- | VkRenderingAttachmentLocationInfoKHR - Structure specifying attachment
-- locations
--
-- = Description
--
-- This structure allows applications to remap the locations of color
-- attachments to different fragment shader output locations.
--
-- Each element of @pColorAttachmentLocations@ set to
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' will be inaccessible to
-- this pipeline as a color attachment; no location will map to it. Each
-- element of @pColorAttachmentLocations@ set to any other value will map
-- the specified location value to the color attachment specified in the
-- render pass at the corresponding index in the
-- @pColorAttachmentLocations@ array. If @pColorAttachmentLocations@ is
-- @NULL@, it is equivalent to setting each element to its index within the
-- array. Any writes to a fragment output location that is not mapped to an
-- attachment /must/ be discarded.
--
-- This structure /can/ be included in the @pNext@ chain of a
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure to set
-- this state for a pipeline. This structure /can/ be included in the
-- @pNext@ chain of a
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' structure to
-- specify inherited state from the primary command buffer. If this
-- structure is not included in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' or
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo', it is
-- equivalent to specifying this structure with the following properties:
--
-- -   @colorAttachmentCount@ set to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@.
--
-- -   @pColorAttachmentLocations@ set to @NULL@.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingAttachmentLocationInfoKHR-dynamicRenderingLocalRead-09512#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, and @pColorAttachmentLocations@ is not
--     @NULL@, each element /must/ be set to the value of its index within
--     the array
--
-- -   #VUID-VkRenderingAttachmentLocationInfoKHR-pColorAttachmentLocations-09513#
--     Elements of @pColorAttachmentLocations@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ each be unique
--
-- -   #VUID-VkRenderingAttachmentLocationInfoKHR-colorAttachmentCount-09514#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxColorAttachments maxColorAttachments>
--
-- -   #VUID-VkRenderingAttachmentLocationInfoKHR-pColorAttachmentLocations-09515#
--     Each element of @pColorAttachmentLocations@ /must/ be less than or
--     equal to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxColorAttachments maxColorAttachments>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingAttachmentLocationInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdSetRenderingAttachmentLocationsKHR'
data RenderingAttachmentLocationInfoKHR = RenderingAttachmentLocationInfoKHR
  { -- | @pColorAttachmentLocations@ is a pointer to an array of
    -- @colorAttachmentCount@ @uint32_t@ values defining remapped locations for
    -- color attachments.
    colorAttachmentLocations :: Vector Word32 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingAttachmentLocationInfoKHR)
#endif
deriving instance Show RenderingAttachmentLocationInfoKHR

instance ToCStruct RenderingAttachmentLocationInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingAttachmentLocationInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentLocations)) :: Word32))
    pPColorAttachmentLocations' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (colorAttachmentLocations)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentLocations' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (colorAttachmentLocations)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPColorAttachmentLocations')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderingAttachmentLocationInfoKHR where
  peekCStruct p = do
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pColorAttachmentLocations <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pColorAttachmentLocations' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Word32 ((pColorAttachmentLocations `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ RenderingAttachmentLocationInfoKHR
             pColorAttachmentLocations'

instance Zero RenderingAttachmentLocationInfoKHR where
  zero = RenderingAttachmentLocationInfoKHR
           mempty


-- | VkRenderingInputAttachmentIndexInfoKHR - Structure specifying input
-- attachment indices
--
-- = Description
--
-- This structure allows applications to remap attachments to different
-- input attachment indices.
--
-- Each element of @pColorAttachmentInputIndices@ set to a value of
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' indicates that the
-- corresponding attachment will not be used as an input attachment in this
-- pipeline. Any other value in each of those elements will map the
-- corresponding attachment to a @InputAttachmentIndex@ value defined in
-- shader code. If @pColorAttachmentInputIndices@ is @NULL@, it is
-- equivalent to setting each element to its index within the array.
--
-- If @pDepthInputAttachmentIndex@ or @pStencilInputAttachmentIndex@ are
-- set to @NULL@, they map to input attachments without a
-- @InputAttachmentIndex@ decoration. If they point to a value of
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', it indicates that the
-- corresponding attachment will not be used as an input attachment in this
-- pipeline. If they point to any other value it maps the corresponding
-- attachment to a @InputAttachmentIndex@ value defined in shader code.
--
-- This structure /can/ be included in the @pNext@ chain of a
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure to set
-- this state for a pipeline. This structure /can/ be included in the
-- @pNext@ chain of a
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' structure to
-- specify inherited state from the primary command buffer. If this
-- structure is not included in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' or
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo', it is
-- equivalent to specifying this structure with the following properties:
--
-- -   @colorAttachmentCount@ set to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@.
--
-- -   @pColorAttachmentInputIndices@ set to @NULL@.
--
-- -   @pDepthInputAttachmentIndex@ set to @NULL@.
--
-- -   @pStencilInputAttachmentIndex@ set to @NULL@.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-dynamicRenderingLocalRead-09519#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, and @pColorAttachmentInputIndices@ is not
--     @NULL@, each element /must/ be set to
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-dynamicRenderingLocalRead-09520#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, @pDepthInputAttachmentIndex@ /must/ be a
--     valid pointer to a value of
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-dynamicRenderingLocalRead-09521#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, @pStencilInputAttachmentIndex@ /must/ be a
--     valid pointer to a value of
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-pColorAttachmentInputIndices-09522#
--     Elements of @pColorAttachmentInputIndices@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ each be unique
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-pColorAttachmentInputIndices-09523#
--     Elements of @pColorAttachmentInputIndices@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ not take the
--     same value as the content of @pDepthInputAttachmentIndex@
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-pColorAttachmentInputIndices-09524#
--     Elements of @pColorAttachmentInputIndices@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ not take the
--     same value as the content of @pStencilInputAttachmentIndex@
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-colorAttachmentCount-09525#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxColorAttachments maxColorAttachments>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR'
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-pColorAttachmentInputIndices-parameter#
--     If @colorAttachmentCount@ is not @0@, and
--     @pColorAttachmentInputIndices@ is not @NULL@,
--     @pColorAttachmentInputIndices@ /must/ be a valid pointer to an array
--     of @colorAttachmentCount@ @uint32_t@ values
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-pDepthInputAttachmentIndex-parameter#
--     If @pDepthInputAttachmentIndex@ is not @NULL@,
--     @pDepthInputAttachmentIndex@ /must/ be a valid pointer to a valid
--     @uint32_t@ value
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfoKHR-pStencilInputAttachmentIndex-parameter#
--     If @pStencilInputAttachmentIndex@ is not @NULL@,
--     @pStencilInputAttachmentIndex@ /must/ be a valid pointer to a valid
--     @uint32_t@ value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdSetRenderingInputAttachmentIndicesKHR'
data RenderingInputAttachmentIndexInfoKHR = RenderingInputAttachmentIndexInfoKHR
  { -- | @colorAttachmentCount@ is the number of elements in
    -- @pColorAttachmentInputIndices@.
    colorAttachmentCount :: Word32
  , -- | @pColorAttachmentInputIndices@ is a pointer to an array of
    -- @colorAttachmentCount@ @uint32_t@ values defining indices for color
    -- attachments to be used as input attachments.
    colorAttachmentInputIndices :: Vector Word32
  , -- | @pDepthInputAttachmentIndex@ is either @NULL@, or a pointer to a
    -- @uint32_t@ value defining the index for the depth attachment to be used
    -- an an input attachment.
    depthInputAttachmentIndex :: Maybe Word32
  , -- | @pStencilInputAttachmentIndex@ is either @NULL@, or a pointer to a
    -- @uint32_t@ value defining the index for the stencil attachment to be
    -- used an an input attachment.
    stencilInputAttachmentIndex :: Maybe Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingInputAttachmentIndexInfoKHR)
#endif
deriving instance Show RenderingInputAttachmentIndexInfoKHR

instance ToCStruct RenderingInputAttachmentIndexInfoKHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingInputAttachmentIndexInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pColorAttachmentInputIndicesLength = Data.Vector.length $ (colorAttachmentInputIndices)
    colorAttachmentCount'' <- lift $ if (colorAttachmentCount) == 0
      then pure $ fromIntegral pColorAttachmentInputIndicesLength
      else do
        unless (fromIntegral pColorAttachmentInputIndicesLength == (colorAttachmentCount) || pColorAttachmentInputIndicesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pColorAttachmentInputIndices must be empty or have 'colorAttachmentCount' elements" Nothing Nothing
        pure (colorAttachmentCount)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (colorAttachmentCount'')
    pColorAttachmentInputIndices'' <- if Data.Vector.null (colorAttachmentInputIndices)
      then pure nullPtr
      else do
        pPColorAttachmentInputIndices <- ContT $ allocaBytes @Word32 (((Data.Vector.length (colorAttachmentInputIndices))) * 4)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentInputIndices `plusPtr` (4 * (i)) :: Ptr Word32) (e)) ((colorAttachmentInputIndices))
        pure $ pPColorAttachmentInputIndices
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) pColorAttachmentInputIndices''
    pDepthInputAttachmentIndex'' <- case (depthInputAttachmentIndex) of
      Nothing -> pure nullPtr
      Just j -> ContT $ with (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word32))) pDepthInputAttachmentIndex''
    pStencilInputAttachmentIndex'' <- case (stencilInputAttachmentIndex) of
      Nothing -> pure nullPtr
      Just j -> ContT $ with (j)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) pStencilInputAttachmentIndex''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderingInputAttachmentIndexInfoKHR where
  peekCStruct p = do
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pColorAttachmentInputIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    let pColorAttachmentInputIndicesLength = if pColorAttachmentInputIndices == nullPtr then 0 else (fromIntegral colorAttachmentCount)
    pColorAttachmentInputIndices' <- generateM pColorAttachmentInputIndicesLength (\i -> peek @Word32 ((pColorAttachmentInputIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pDepthInputAttachmentIndex <- peek @(Ptr Word32) ((p `plusPtr` 32 :: Ptr (Ptr Word32)))
    pDepthInputAttachmentIndex' <- maybePeek (\j -> peek @Word32 (j)) pDepthInputAttachmentIndex
    pStencilInputAttachmentIndex <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pStencilInputAttachmentIndex' <- maybePeek (\j -> peek @Word32 (j)) pStencilInputAttachmentIndex
    pure $ RenderingInputAttachmentIndexInfoKHR
             colorAttachmentCount
             pColorAttachmentInputIndices'
             pDepthInputAttachmentIndex'
             pStencilInputAttachmentIndex'

instance Zero RenderingInputAttachmentIndexInfoKHR where
  zero = RenderingInputAttachmentIndexInfoKHR
           zero
           mempty
           Nothing
           Nothing


type KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION"
pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION = 1


type KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME = "VK_KHR_dynamic_rendering_local_read"

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME"
pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME = "VK_KHR_dynamic_rendering_local_read"

