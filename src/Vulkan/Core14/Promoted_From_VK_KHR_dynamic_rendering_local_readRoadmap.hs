{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap  ( cmdSetRenderingAttachmentLocations
                                                                               , cmdSetRenderingInputAttachmentIndices
                                                                               , PhysicalDeviceDynamicRenderingLocalReadFeatures(..)
                                                                               , RenderingAttachmentLocationInfo(..)
                                                                               , RenderingInputAttachmentIndexInfo(..)
                                                                               , ImageLayout(..)
                                                                               , StructureType(..)
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
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRenderingAttachmentLocations))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRenderingInputAttachmentIndices))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRenderingAttachmentLocations
  :: FunPtr (Ptr CommandBuffer_T -> Ptr RenderingAttachmentLocationInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr RenderingAttachmentLocationInfo -> IO ()

-- | vkCmdSetRenderingAttachmentLocations - Set color attachment location
-- mappings for a command buffer
--
-- = Description
--
-- This command sets the attachment location mappings for subsequent
-- drawing commands, and /must/ match the mappings provided to the bound
-- pipeline, if one is bound, which /can/ be set by chaining
-- 'RenderingAttachmentLocationInfo' to
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'.
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
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-dynamicRenderingLocalRead-09509#
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     /must/ be enabled
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-pLocationInfo-09510#
--     @pLocationInfo->colorAttachmentCount@ /must/ be equal to the value
--     of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     used to begin the current render pass instance
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-commandBuffer-09511# The
--     current render pass instance /must/ have been started or resumed by
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     in this @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-pLocationInfo-parameter#
--     @pLocationInfo@ /must/ be a valid pointer to a valid
--     'RenderingAttachmentLocationInfo' structure
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-renderpass# This command
--     /must/ only be called inside of a render pass instance
--
-- -   #VUID-vkCmdSetRenderingAttachmentLocations-videocoding# This command
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSetRenderingAttachmentLocations is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'RenderingAttachmentLocationInfo'
cmdSetRenderingAttachmentLocations :: forall io
                                    . (MonadIO io)
                                   => -- | @commandBuffer@ is the command buffer into which the command will be
                                      -- recorded.
                                      CommandBuffer
                                   -> -- | @pLocationInfo@ is a 'RenderingAttachmentLocationInfo' structure
                                      -- indicating the new mappings.
                                      RenderingAttachmentLocationInfo
                                   -> io ()
cmdSetRenderingAttachmentLocations commandBuffer
                                     locationInfo = liftIO . evalContT $ do
  let vkCmdSetRenderingAttachmentLocationsPtr = pVkCmdSetRenderingAttachmentLocations (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetRenderingAttachmentLocationsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRenderingAttachmentLocations is null" Nothing Nothing
  let vkCmdSetRenderingAttachmentLocations' = mkVkCmdSetRenderingAttachmentLocations vkCmdSetRenderingAttachmentLocationsPtr
  pLocationInfo <- ContT $ withCStruct (locationInfo)
  lift $ traceAroundEvent "vkCmdSetRenderingAttachmentLocations" (vkCmdSetRenderingAttachmentLocations'
                                                                    (commandBufferHandle (commandBuffer))
                                                                    pLocationInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRenderingInputAttachmentIndices
  :: FunPtr (Ptr CommandBuffer_T -> Ptr RenderingInputAttachmentIndexInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr RenderingInputAttachmentIndexInfo -> IO ()

-- | vkCmdSetRenderingInputAttachmentIndices - Set input attachment index
-- mappings for a command buffer
--
-- = Description
--
-- This command sets the input attachment index mappings for subsequent
-- drawing commands, and /must/ match the mappings provided to the bound
-- pipeline, if one is bound, which /can/ be set by chaining
-- 'RenderingInputAttachmentIndexInfo' to
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'.
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
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-dynamicRenderingLocalRead-09516#
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     /must/ be enabled
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-pInputAttachmentIndexInfo-09517#
--     @pInputAttachmentIndexInfo->colorAttachmentCount@ /must/ be equal to
--     the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     used to begin the current render pass instance
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-commandBuffer-09518#
--     The current render pass instance /must/ have been started or resumed
--     by
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     in this @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-pInputAttachmentIndexInfo-parameter#
--     @pInputAttachmentIndexInfo@ /must/ be a valid pointer to a valid
--     'RenderingInputAttachmentIndexInfo' structure
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-renderpass# This
--     command /must/ only be called inside of a render pass instance
--
-- -   #VUID-vkCmdSetRenderingInputAttachmentIndices-videocoding# This
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSetRenderingInputAttachmentIndices is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'RenderingInputAttachmentIndexInfo'
cmdSetRenderingInputAttachmentIndices :: forall io
                                       . (MonadIO io)
                                      => -- | @commandBuffer@ is the command buffer into which the command will be
                                         -- recorded.
                                         CommandBuffer
                                      -> -- | @pInputAttachmentIndexInfo@ is a 'RenderingInputAttachmentIndexInfo'
                                         -- structure indicating the new mappings.
                                         RenderingInputAttachmentIndexInfo
                                      -> io ()
cmdSetRenderingInputAttachmentIndices commandBuffer
                                        inputAttachmentIndexInfo = liftIO . evalContT $ do
  let vkCmdSetRenderingInputAttachmentIndicesPtr = pVkCmdSetRenderingInputAttachmentIndices (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetRenderingInputAttachmentIndicesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRenderingInputAttachmentIndices is null" Nothing Nothing
  let vkCmdSetRenderingInputAttachmentIndices' = mkVkCmdSetRenderingInputAttachmentIndices vkCmdSetRenderingInputAttachmentIndicesPtr
  pInputAttachmentIndexInfo <- ContT $ withCStruct (inputAttachmentIndexInfo)
  lift $ traceAroundEvent "vkCmdSetRenderingInputAttachmentIndices" (vkCmdSetRenderingInputAttachmentIndices'
                                                                       (commandBufferHandle (commandBuffer))
                                                                       pInputAttachmentIndexInfo)
  pure $ ()


-- | VkPhysicalDeviceDynamicRenderingLocalReadFeatures - Structure indicating
-- support for local reads in dynamic render pass instances
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDynamicRenderingLocalReadFeatures' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDynamicRenderingLocalReadFeatures', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDynamicRenderingLocalReadFeatures = PhysicalDeviceDynamicRenderingLocalReadFeatures
  { -- | #extension-features-dynamicRenderingLocalRead#
    -- @dynamicRenderingLocalRead@ specifies that the implementation supports
    -- local reads inside dynamic render pass instances using the
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
    -- command.
    dynamicRenderingLocalRead :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDynamicRenderingLocalReadFeatures)
#endif
deriving instance Show PhysicalDeviceDynamicRenderingLocalReadFeatures

instance ToCStruct PhysicalDeviceDynamicRenderingLocalReadFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDynamicRenderingLocalReadFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dynamicRenderingLocalRead))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDynamicRenderingLocalReadFeatures where
  peekCStruct p = do
    dynamicRenderingLocalRead <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDynamicRenderingLocalReadFeatures
             (bool32ToBool dynamicRenderingLocalRead)

instance Storable PhysicalDeviceDynamicRenderingLocalReadFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDynamicRenderingLocalReadFeatures where
  zero = PhysicalDeviceDynamicRenderingLocalReadFeatures
           zero


-- | VkRenderingAttachmentLocationInfo - Structure specifying attachment
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
-- @pColorAttachmentLocations@ array. Any writes to a fragment output
-- location that is not mapped to an attachment /must/ be discarded.
--
-- If @pColorAttachmentLocations@ is @NULL@, it is equivalent to setting
-- each element to its index within the array.
--
-- This structure /can/ be included in the @pNext@ chain of a
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo' structure to
-- set this state for a pipeline. If this structure is not included in the
-- @pNext@ chain of
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo', it is
-- equivalent to specifying this structure with the following properties:
--
-- -   @colorAttachmentCount@ set to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@.
--
-- -   @pColorAttachmentLocations@ set to @NULL@.
--
-- This structure /can/ be included in the @pNext@ chain of a
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' structure to
-- specify inherited state from the primary command buffer. If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::renderPass
-- is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', or
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is not specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::flags, members of
-- this structure are ignored. If this structure is not included in the
-- @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo', it is
-- equivalent to specifying this structure with the following properties:
--
-- -   @colorAttachmentCount@ set to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'::@colorAttachmentCount@.
--
-- -   @pColorAttachmentLocations@ set to @NULL@.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingAttachmentLocationInfo-dynamicRenderingLocalRead-09512#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, and @pColorAttachmentLocations@ is not
--     @NULL@, each element /must/ be the value of its index within the
--     array
--
-- -   #VUID-VkRenderingAttachmentLocationInfo-pColorAttachmentLocations-09513#
--     Elements of @pColorAttachmentLocations@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ each be unique
--
-- -   #VUID-VkRenderingAttachmentLocationInfo-colorAttachmentCount-09514#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxColorAttachments maxColorAttachments>
--
-- -   #VUID-VkRenderingAttachmentLocationInfo-pColorAttachmentLocations-09515#
--     Each element of @pColorAttachmentLocations@ /must/ be less than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxColorAttachments maxColorAttachments>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingAttachmentLocationInfo-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdSetRenderingAttachmentLocations',
-- 'cmdSetRenderingAttachmentLocations'
data RenderingAttachmentLocationInfo = RenderingAttachmentLocationInfo
  { -- | @pColorAttachmentLocations@ is a pointer to an array of
    -- @colorAttachmentCount@ @uint32_t@ values defining remapped locations for
    -- color attachments.
    colorAttachmentLocations :: Vector Word32 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingAttachmentLocationInfo)
#endif
deriving instance Show RenderingAttachmentLocationInfo

instance ToCStruct RenderingAttachmentLocationInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingAttachmentLocationInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentLocations)) :: Word32))
    pPColorAttachmentLocations' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (colorAttachmentLocations)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentLocations' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (colorAttachmentLocations)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPColorAttachmentLocations')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderingAttachmentLocationInfo where
  peekCStruct p = do
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pColorAttachmentLocations <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pColorAttachmentLocations' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Word32 ((pColorAttachmentLocations `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ RenderingAttachmentLocationInfo
             pColorAttachmentLocations'

instance Zero RenderingAttachmentLocationInfo where
  zero = RenderingAttachmentLocationInfo
           mempty


-- | VkRenderingInputAttachmentIndexInfo - Structure specifying input
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
-- shader code.
--
-- If @pColorAttachmentInputIndices@ is @NULL@, it is equivalent to setting
-- each element to its index within the array.
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
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo' structure to
-- set this state for a pipeline. If this structure is not included in the
-- @pNext@ chain of
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo', it is
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
-- This structure /can/ be included in the @pNext@ chain of a
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' structure to
-- specify inherited state from the primary command buffer. If this
-- structure is not included in the @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo', it is
-- equivalent to specifying this structure with the following properties:
--
-- -   @colorAttachmentCount@ set to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'::@colorAttachmentCount@.
--
-- -   @pColorAttachmentInputIndices@ set to @NULL@.
--
-- -   @pDepthInputAttachmentIndex@ set to @NULL@.
--
-- -   @pStencilInputAttachmentIndex@ set to @NULL@.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-dynamicRenderingLocalRead-09519#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, and @pColorAttachmentInputIndices@ is not
--     @NULL@, each element /must/ be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-dynamicRenderingLocalRead-09520#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, @pDepthInputAttachmentIndex@ /must/ be a
--     valid pointer to a value of
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-dynamicRenderingLocalRead-09521#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, @pStencilInputAttachmentIndex@ /must/ be a
--     valid pointer to a value of
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-pColorAttachmentInputIndices-09522#
--     Elements of @pColorAttachmentInputIndices@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ each be unique
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-pColorAttachmentInputIndices-09523#
--     Elements of @pColorAttachmentInputIndices@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ not take the
--     same value as the content of @pDepthInputAttachmentIndex@
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-pColorAttachmentInputIndices-09524#
--     Elements of @pColorAttachmentInputIndices@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ not take the
--     same value as the content of @pStencilInputAttachmentIndex@
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-colorAttachmentCount-09525#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxColorAttachments maxColorAttachments>
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-pDepthInputAttachmentIndex-12274#
--     Elements of @pDepthInputAttachmentIndex@,
--     @pStencilInputAttachmentIndex@, and @pColorAttachmentInputIndices@
--     that are not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/
--     be less than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxPerStageDescriptorInputAttachments maxPerStageDescriptorInputAttachments>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO'
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-pColorAttachmentInputIndices-parameter#
--     If @colorAttachmentCount@ is not @0@, and
--     @pColorAttachmentInputIndices@ is not @NULL@,
--     @pColorAttachmentInputIndices@ /must/ be a valid pointer to an array
--     of @colorAttachmentCount@ @uint32_t@ values
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-pDepthInputAttachmentIndex-parameter#
--     If @pDepthInputAttachmentIndex@ is not @NULL@,
--     @pDepthInputAttachmentIndex@ /must/ be a valid pointer to a valid
--     @uint32_t@ value
--
-- -   #VUID-VkRenderingInputAttachmentIndexInfo-pStencilInputAttachmentIndex-parameter#
--     If @pStencilInputAttachmentIndex@ is not @NULL@,
--     @pStencilInputAttachmentIndex@ /must/ be a valid pointer to a valid
--     @uint32_t@ value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdSetRenderingInputAttachmentIndices',
-- 'cmdSetRenderingInputAttachmentIndices'
data RenderingInputAttachmentIndexInfo = RenderingInputAttachmentIndexInfo
  { -- | @colorAttachmentCount@ is the number of elements in
    -- @pColorAttachmentInputIndices@.
    colorAttachmentCount :: Word32
  , -- | @pColorAttachmentInputIndices@ is a pointer to an array of
    -- @colorAttachmentCount@ @uint32_t@ values defining indices for color
    -- attachments to be used as input attachments.
    colorAttachmentInputIndices :: Vector Word32
  , -- | @pDepthInputAttachmentIndex@ is either @NULL@, or a pointer to a
    -- @uint32_t@ value defining the index for the depth attachment to be used
    -- as an input attachment.
    depthInputAttachmentIndex :: Maybe Word32
  , -- | @pStencilInputAttachmentIndex@ is either @NULL@, or a pointer to a
    -- @uint32_t@ value defining the index for the stencil attachment to be
    -- used as an input attachment.
    stencilInputAttachmentIndex :: Maybe Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingInputAttachmentIndexInfo)
#endif
deriving instance Show RenderingInputAttachmentIndexInfo

instance ToCStruct RenderingInputAttachmentIndexInfo where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingInputAttachmentIndexInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderingInputAttachmentIndexInfo where
  peekCStruct p = do
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pColorAttachmentInputIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    let pColorAttachmentInputIndicesLength = if pColorAttachmentInputIndices == nullPtr then 0 else (fromIntegral colorAttachmentCount)
    pColorAttachmentInputIndices' <- generateM pColorAttachmentInputIndicesLength (\i -> peek @Word32 ((pColorAttachmentInputIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pDepthInputAttachmentIndex <- peek @(Ptr Word32) ((p `plusPtr` 32 :: Ptr (Ptr Word32)))
    pDepthInputAttachmentIndex' <- maybePeek (\j -> peek @Word32 (j)) pDepthInputAttachmentIndex
    pStencilInputAttachmentIndex <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pStencilInputAttachmentIndex' <- maybePeek (\j -> peek @Word32 (j)) pStencilInputAttachmentIndex
    pure $ RenderingInputAttachmentIndexInfo
             colorAttachmentCount
             pColorAttachmentInputIndices'
             pDepthInputAttachmentIndex'
             pStencilInputAttachmentIndex'

instance Zero RenderingInputAttachmentIndexInfo where
  zero = RenderingInputAttachmentIndexInfo
           zero
           mempty
           Nothing
           Nothing

