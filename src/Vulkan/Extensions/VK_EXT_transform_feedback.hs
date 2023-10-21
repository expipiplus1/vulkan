{-# language CPP #-}
-- | = Name
--
-- VK_EXT_transform_feedback - device extension
--
-- == VK_EXT_transform_feedback
--
-- [__Name String__]
--     @VK_EXT_transform_feedback@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     29
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Uses__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_transform_feedback] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_transform_feedback extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-09
--
-- [__Contributors__]
--
--     -   Baldur Karlsson, Valve
--
--     -   Boris Zanin, Mobica
--
--     -   Daniel Rakos, AMD
--
--     -   Donald Scorgie, Imagination
--
--     -   Henri Verbeet, CodeWeavers
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Faith Ekstrand, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Barker, Unity
--
--     -   Jesse Hall, Google
--
--     -   Pierre-Loup Griffais, Valve
--
--     -   Philip Rebohle, DXVK
--
--     -   Ruihao Zhang, Qualcomm
--
--     -   Samuel Pitoiset, Valve
--
--     -   Slawomir Grajewski, Intel
--
--     -   Stu Smith, Imagination Technologies
--
-- == Description
--
-- This extension adds transform feedback to the Vulkan API by exposing the
-- SPIR-V @TransformFeedback@ and @GeometryStreams@ capabilities to capture
-- vertex, tessellation or geometry shader outputs to one or more buffers.
-- It adds API functionality to bind transform feedback buffers to capture
-- the primitives emitted by the graphics pipeline from SPIR-V outputs
-- decorated for transform feedback. The transform feedback capture can be
-- paused and resumed by way of storing and retrieving a byte counter. The
-- captured data can be drawn again where the vertex count is derived from
-- the byte counter without CPU intervention. If the implementation is
-- capable, a vertex stream other than zero can be rasterized.
--
-- All these features are designed to match the full capabilities of OpenGL
-- core transform feedback functionality and beyond. Many of the features
-- are optional to allow base OpenGL ES GPUs to also implement this
-- extension.
--
-- The primary purpose of the functionality exposed by this extension is to
-- support translation layers from other 3D APIs. This functionality is not
-- considered forward looking, and is not expected to be promoted to a KHR
-- extension or to core Vulkan. Unless this is needed for translation, it
-- is recommended that developers use alternative techniques of using the
-- GPU to process and capture vertex data.
--
-- == New Commands
--
-- -   'cmdBeginQueryIndexedEXT'
--
-- -   'cmdBeginTransformFeedbackEXT'
--
-- -   'cmdBindTransformFeedbackBuffersEXT'
--
-- -   'cmdDrawIndirectByteCountEXT'
--
-- -   'cmdEndQueryIndexedEXT'
--
-- -   'cmdEndTransformFeedbackEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTransformFeedbackFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTransformFeedbackPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationStateStreamCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'PipelineRasterizationStateStreamCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME'
--
-- -   'EXT_TRANSFORM_FEEDBACK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) Should we include pause\/resume functionality?
--
-- __RESOLVED__: Yes, this is needed to ease layering other APIs which have
-- this functionality. To pause use 'cmdEndTransformFeedbackEXT' and
-- provide valid buffer handles in the @pCounterBuffers@ array and offsets
-- in the @pCounterBufferOffsets@ array for the implementation to save the
-- resume points. Then to resume use 'cmdBeginTransformFeedbackEXT' with
-- the previous @pCounterBuffers@ and @pCounterBufferOffsets@ values.
-- Between the pause and resume there needs to be a memory barrier for the
-- counter buffers with a source access of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT'
-- at pipeline stage
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
-- to a destination access of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT'
-- at pipeline stage
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'.
--
-- 2) How does this interact with multiview?
--
-- __RESOLVED__: Transform feedback cannot be made active in a render pass
-- with multiview enabled.
--
-- 3) How should queries be done?
--
-- __RESOLVED__: There is a new query type
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'.
-- A query pool created with this type will capture 2 integers -
-- numPrimitivesWritten and numPrimitivesNeeded - for the specified vertex
-- stream output from the last
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>.
-- The vertex stream output queried is zero by default, but can be
-- specified with the new 'cmdBeginQueryIndexedEXT' and
-- 'cmdEndQueryIndexedEXT' commands.
--
-- == Version History
--
-- -   Revision 1, 2018-10-09 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceTransformFeedbackFeaturesEXT',
-- 'PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'PipelineRasterizationStateStreamCreateFlagsEXT',
-- 'PipelineRasterizationStateStreamCreateInfoEXT',
-- 'cmdBeginQueryIndexedEXT', 'cmdBeginTransformFeedbackEXT',
-- 'cmdBindTransformFeedbackBuffersEXT', 'cmdDrawIndirectByteCountEXT',
-- 'cmdEndQueryIndexedEXT', 'cmdEndTransformFeedbackEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_transform_feedback Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_transform_feedback  ( cmdBindTransformFeedbackBuffersEXT
                                                    , cmdBeginTransformFeedbackEXT
                                                    , cmdUseTransformFeedbackEXT
                                                    , cmdEndTransformFeedbackEXT
                                                    , cmdBeginQueryIndexedEXT
                                                    , cmdUseQueryIndexedEXT
                                                    , cmdEndQueryIndexedEXT
                                                    , cmdDrawIndirectByteCountEXT
                                                    , PhysicalDeviceTransformFeedbackFeaturesEXT(..)
                                                    , PhysicalDeviceTransformFeedbackPropertiesEXT(..)
                                                    , PipelineRasterizationStateStreamCreateInfoEXT(..)
                                                    , PipelineRasterizationStateStreamCreateFlagsEXT(..)
                                                    , EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
                                                    , pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
                                                    , EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
                                                    , pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
                                                    ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginQueryIndexedEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginTransformFeedbackEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindTransformFeedbackBuffersEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndirectByteCountEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndQueryIndexedEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndTransformFeedbackEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlagBits(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindTransformFeedbackBuffersEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()

-- | vkCmdBindTransformFeedbackBuffersEXT - Bind transform feedback buffers
-- to a command buffer
--
-- = Description
--
-- The values taken from elements i of @pBuffers@, @pOffsets@ and @pSizes@
-- replace the current state for the transform feedback binding
-- @firstBinding@ + i, for i in [0, @bindingCount@). The transform feedback
-- binding is updated to start at the offset indicated by @pOffsets@[i]
-- from the start of the buffer @pBuffers@[i].
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-transformFeedback-02355#
--     'PhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-firstBinding-02356#
--     @firstBinding@ /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-firstBinding-02357# The
--     sum of @firstBinding@ and @bindingCount@ /must/ be less than or
--     equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pOffsets-02358# All
--     elements of @pOffsets@ /must/ be less than the size of the
--     corresponding element in @pBuffers@
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pOffsets-02359# All
--     elements of @pOffsets@ /must/ be a multiple of 4
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pBuffers-02360# All
--     elements of @pBuffers@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT'
--     flag
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pSize-02361# If the
--     optional @pSize@ array is specified, each element of @pSizes@ /must/
--     either be 'Vulkan.Core10.APIConstants.WHOLE_SIZE', or be less than
--     or equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBufferSize@
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pSizes-02362# All
--     elements of @pSizes@ /must/ be either
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', or less than or equal to
--     the size of the corresponding buffer in @pBuffers@
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pOffsets-02363# All
--     elements of @pOffsets@ plus @pSizes@, where the @pSizes@, element is
--     not 'Vulkan.Core10.APIConstants.WHOLE_SIZE', /must/ be less than or
--     equal to the size of the corresponding buffer in @pBuffers@
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pBuffers-02364# Each
--     element of @pBuffers@ that is non-sparse /must/ be bound completely
--     and contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory'
--     object
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-None-02365# Transform
--     feedback /must/ not be active when the
--     'cmdBindTransformFeedbackBuffersEXT' command is recorded
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pBuffers-parameter#
--     @pBuffers@ /must/ be a valid pointer to an array of @bindingCount@
--     valid 'Vulkan.Core10.Handles.Buffer' handles
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-pOffsets-parameter#
--     @pOffsets@ /must/ be a valid pointer to an array of @bindingCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-bindingCount-arraylength#
--     @bindingCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdBindTransformFeedbackBuffersEXT-commonparent# Both of
--     @commandBuffer@, and the elements of @pBuffers@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdBindTransformFeedbackBuffersEXT :: forall io
                                    . (MonadIO io)
                                   => -- | @commandBuffer@ is the command buffer into which the command is
                                      -- recorded.
                                      CommandBuffer
                                   -> -- | @firstBinding@ is the index of the first transform feedback binding
                                      -- whose state is updated by the command.
                                      ("firstBinding" ::: Word32)
                                   -> -- | @pBuffers@ is a pointer to an array of buffer handles.
                                      ("buffers" ::: Vector Buffer)
                                   -> -- | @pOffsets@ is a pointer to an array of buffer offsets.
                                      ("offsets" ::: Vector DeviceSize)
                                   -> -- | @pSizes@ is @NULL@ or a pointer to an array of
                                      -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' buffer sizes, specifying the
                                      -- maximum number of bytes to capture to the corresponding transform
                                      -- feedback buffer. If @pSizes@ is @NULL@, or the value of the @pSizes@
                                      -- array element is 'Vulkan.Core10.APIConstants.WHOLE_SIZE', then the
                                      -- maximum number of bytes captured will be the size of the corresponding
                                      -- buffer minus the buffer offset.
                                      ("sizes" ::: Vector DeviceSize)
                                   -> io ()
cmdBindTransformFeedbackBuffersEXT commandBuffer
                                     firstBinding
                                     buffers
                                     offsets
                                     sizes = liftIO . evalContT $ do
  let vkCmdBindTransformFeedbackBuffersEXTPtr = pVkCmdBindTransformFeedbackBuffersEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindTransformFeedbackBuffersEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindTransformFeedbackBuffersEXT is null" Nothing Nothing
  let vkCmdBindTransformFeedbackBuffersEXT' = mkVkCmdBindTransformFeedbackBuffersEXT vkCmdBindTransformFeedbackBuffersEXTPtr
  let pBuffersLength = Data.Vector.length $ (buffers)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  let pSizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral pSizesLength == pBuffersLength || pSizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pSizes and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytes @Buffer ((Data.Vector.length (buffers)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytes @DeviceSize ((Data.Vector.length (offsets)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  pSizes <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pPSizes <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (sizes))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pPSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pPSizes
  lift $ traceAroundEvent "vkCmdBindTransformFeedbackBuffersEXT" (vkCmdBindTransformFeedbackBuffersEXT'
                                                                    (commandBufferHandle (commandBuffer))
                                                                    (firstBinding)
                                                                    ((fromIntegral pBuffersLength :: Word32))
                                                                    (pPBuffers)
                                                                    (pPOffsets)
                                                                    pSizes)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginTransformFeedbackEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- | vkCmdBeginTransformFeedbackEXT - Make transform feedback active in the
-- command buffer
--
-- = Description
--
-- The active transform feedback buffers will capture primitives emitted
-- from the corresponding @XfbBuffer@ in the bound graphics pipeline. Any
-- @XfbBuffer@ emitted that does not output to an active transform feedback
-- buffer will not be captured.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-transformFeedback-02366#
--     'PhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-None-02367# Transform feedback
--     /must/ not be active
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-firstCounterBuffer-02368#
--     @firstCounterBuffer@ /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-firstCounterBuffer-02369# The
--     sum of @firstCounterBuffer@ and @counterBufferCount@ /must/ be less
--     than or equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-counterBufferCount-02607# If
--     @counterBufferCount@ is not @0@, and @pCounterBuffers@ is not
--     @NULL@, @pCounterBuffers@ /must/ be a valid pointer to an array of
--     @counterBufferCount@ 'Vulkan.Core10.Handles.Buffer' handles that are
--     either valid or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-pCounterBufferOffsets-02370#
--     For each buffer handle in the array, if it is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ reference a
--     buffer large enough to hold 4 bytes at the corresponding offset from
--     the @pCounterBufferOffsets@ array
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-pCounterBuffer-02371# If
--     @pCounterBuffer@ is @NULL@, then @pCounterBufferOffsets@ /must/ also
--     be @NULL@
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-pCounterBuffers-02372# For each
--     buffer handle in the @pCounterBuffers@ array that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ have been created
--     with a @usage@ value containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-None-06233# A valid graphics
--     pipeline /must/ be bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-None-04128# The last
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>
--     of the bound graphics pipeline /must/ have been declared with the
--     @Xfb@ execution mode
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-None-02373# Transform feedback
--     /must/ not be made active in a render pass instance with multiview
--     enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-pCounterBufferOffsets-parameter#
--     If @counterBufferCount@ is not @0@, and @pCounterBufferOffsets@ is
--     not @NULL@, @pCounterBufferOffsets@ /must/ be a valid pointer to an
--     array of @counterBufferCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-renderpass# This command /must/
--     only be called inside of a render pass instance
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-commonparent# Both of
--     @commandBuffer@, and the elements of @pCounterBuffers@ that are
--     valid handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdBeginTransformFeedbackEXT :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer into which the command is
                                -- recorded.
                                CommandBuffer
                             -> -- | @firstCounterBuffer@ is the index of the first transform feedback buffer
                                -- corresponding to @pCounterBuffers@[0] and @pCounterBufferOffsets@[0].
                                ("firstCounterBuffer" ::: Word32)
                             -> -- | @pCounterBuffers@ is @NULL@ or a pointer to an array of
                                -- 'Vulkan.Core10.Handles.Buffer' handles to counter buffers. Each buffer
                                -- contains a 4 byte integer value representing the byte offset from the
                                -- start of the corresponding transform feedback buffer from where to start
                                -- capturing vertex data. If the byte offset stored to the counter buffer
                                -- location was done using 'cmdEndTransformFeedbackEXT' it can be used to
                                -- resume transform feedback from the previous location. If
                                -- @pCounterBuffers@ is @NULL@, then transform feedback will start
                                -- capturing vertex data to byte offset zero in all bound transform
                                -- feedback buffers. For each element of @pCounterBuffers@ that is
                                -- 'Vulkan.Core10.APIConstants.NULL_HANDLE', transform feedback will start
                                -- capturing vertex data to byte zero in the corresponding bound transform
                                -- feedback buffer.
                                ("counterBuffers" ::: Vector Buffer)
                             -> -- | @pCounterBufferOffsets@ is @NULL@ or a pointer to an array of
                                -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' values specifying offsets
                                -- within each of the @pCounterBuffers@ where the counter values were
                                -- previously written. The location in each counter buffer at these offsets
                                -- /must/ be large enough to contain 4 bytes of data. This data is the
                                -- number of bytes captured by the previous transform feedback to this
                                -- buffer. If @pCounterBufferOffsets@ is @NULL@, then it is assumed the
                                -- offsets are zero.
                                ("counterBufferOffsets" ::: Vector DeviceSize)
                             -> io ()
cmdBeginTransformFeedbackEXT commandBuffer
                               firstCounterBuffer
                               counterBuffers
                               counterBufferOffsets = liftIO . evalContT $ do
  let vkCmdBeginTransformFeedbackEXTPtr = pVkCmdBeginTransformFeedbackEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBeginTransformFeedbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginTransformFeedbackEXT is null" Nothing Nothing
  let vkCmdBeginTransformFeedbackEXT' = mkVkCmdBeginTransformFeedbackEXT vkCmdBeginTransformFeedbackEXTPtr
  let pCounterBuffersLength = Data.Vector.length $ (counterBuffers)
  let pCounterBufferOffsetsLength = Data.Vector.length $ (counterBufferOffsets)
  lift $ unless (fromIntegral pCounterBufferOffsetsLength == pCounterBuffersLength || pCounterBufferOffsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pCounterBufferOffsets and pCounterBuffers must have the same length" Nothing Nothing
  pPCounterBuffers <- ContT $ allocaBytes @Buffer ((Data.Vector.length (counterBuffers)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (counterBuffers)
  pCounterBufferOffsets <- if Data.Vector.null (counterBufferOffsets)
    then pure nullPtr
    else do
      pPCounterBufferOffsets <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (counterBufferOffsets))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBufferOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((counterBufferOffsets))
      pure $ pPCounterBufferOffsets
  lift $ traceAroundEvent "vkCmdBeginTransformFeedbackEXT" (vkCmdBeginTransformFeedbackEXT'
                                                              (commandBufferHandle (commandBuffer))
                                                              (firstCounterBuffer)
                                                              ((fromIntegral pCounterBuffersLength :: Word32))
                                                              (pPCounterBuffers)
                                                              pCounterBufferOffsets)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginTransformFeedbackEXT' and 'cmdEndTransformFeedbackEXT'
--
-- Note that 'cmdEndTransformFeedbackEXT' is *not* called if an exception
-- is thrown by the inner action.
cmdUseTransformFeedbackEXT :: forall io r . MonadIO io => CommandBuffer -> Word32 -> Vector Buffer -> Vector DeviceSize -> io r -> io r
cmdUseTransformFeedbackEXT commandBuffer
                             firstCounterBuffer
                             pCounterBuffers
                             pCounterBufferOffsets
                             a =
  (cmdBeginTransformFeedbackEXT commandBuffer
                                  firstCounterBuffer
                                  pCounterBuffers
                                  pCounterBufferOffsets) *> a <* (cmdEndTransformFeedbackEXT commandBuffer
                                                                                               firstCounterBuffer
                                                                                               pCounterBuffers
                                                                                               pCounterBufferOffsets)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndTransformFeedbackEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- | vkCmdEndTransformFeedbackEXT - Make transform feedback inactive in the
-- command buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-transformFeedback-02374#
--     'PhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-None-02375# Transform feedback
--     /must/ be active
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-firstCounterBuffer-02376#
--     @firstCounterBuffer@ /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-firstCounterBuffer-02377# The sum
--     of @firstCounterBuffer@ and @counterBufferCount@ /must/ be less than
--     or equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-counterBufferCount-02608# If
--     @counterBufferCount@ is not @0@, and @pCounterBuffers@ is not
--     @NULL@, @pCounterBuffers@ /must/ be a valid pointer to an array of
--     @counterBufferCount@ 'Vulkan.Core10.Handles.Buffer' handles that are
--     either valid or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-pCounterBufferOffsets-02378# For
--     each buffer handle in the array, if it is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ reference a
--     buffer large enough to hold 4 bytes at the corresponding offset from
--     the @pCounterBufferOffsets@ array
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-pCounterBuffer-02379# If
--     @pCounterBuffer@ is @NULL@, then @pCounterBufferOffsets@ /must/ also
--     be @NULL@
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-pCounterBuffers-02380# For each
--     buffer handle in the @pCounterBuffers@ array that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ have been created
--     with a @usage@ value containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-pCounterBufferOffsets-parameter#
--     If @counterBufferCount@ is not @0@, and @pCounterBufferOffsets@ is
--     not @NULL@, @pCounterBufferOffsets@ /must/ be a valid pointer to an
--     array of @counterBufferCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-renderpass# This command /must/
--     only be called inside of a render pass instance
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdEndTransformFeedbackEXT-commonparent# Both of
--     @commandBuffer@, and the elements of @pCounterBuffers@ that are
--     valid handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdEndTransformFeedbackEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command is
                              -- recorded.
                              CommandBuffer
                           -> -- | @firstCounterBuffer@ is the index of the first transform feedback buffer
                              -- corresponding to @pCounterBuffers@[0] and @pCounterBufferOffsets@[0].
                              ("firstCounterBuffer" ::: Word32)
                           -> -- | @pCounterBuffers@ is @NULL@ or a pointer to an array of
                              -- 'Vulkan.Core10.Handles.Buffer' handles to counter buffers. The counter
                              -- buffers are used to record the current byte positions of each transform
                              -- feedback buffer where the next vertex output data would be captured.
                              -- This /can/ be used by a subsequent 'cmdBeginTransformFeedbackEXT' call
                              -- to resume transform feedback capture from this position. It can also be
                              -- used by 'cmdDrawIndirectByteCountEXT' to determine the vertex count of
                              -- the draw call.
                              ("counterBuffers" ::: Vector Buffer)
                           -> -- | @pCounterBufferOffsets@ is @NULL@ or a pointer to an array of
                              -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' values specifying offsets
                              -- within each of the @pCounterBuffers@ where the counter values can be
                              -- written. The location in each counter buffer at these offsets /must/ be
                              -- large enough to contain 4 bytes of data. The data stored at this
                              -- location is the byte offset from the start of the transform feedback
                              -- buffer binding where the next vertex data would be written. If
                              -- @pCounterBufferOffsets@ is @NULL@, then it is assumed the offsets are
                              -- zero.
                              ("counterBufferOffsets" ::: Vector DeviceSize)
                           -> io ()
cmdEndTransformFeedbackEXT commandBuffer
                             firstCounterBuffer
                             counterBuffers
                             counterBufferOffsets = liftIO . evalContT $ do
  let vkCmdEndTransformFeedbackEXTPtr = pVkCmdEndTransformFeedbackEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdEndTransformFeedbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndTransformFeedbackEXT is null" Nothing Nothing
  let vkCmdEndTransformFeedbackEXT' = mkVkCmdEndTransformFeedbackEXT vkCmdEndTransformFeedbackEXTPtr
  let pCounterBuffersLength = Data.Vector.length $ (counterBuffers)
  let pCounterBufferOffsetsLength = Data.Vector.length $ (counterBufferOffsets)
  lift $ unless (fromIntegral pCounterBufferOffsetsLength == pCounterBuffersLength || pCounterBufferOffsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pCounterBufferOffsets and pCounterBuffers must have the same length" Nothing Nothing
  pPCounterBuffers <- ContT $ allocaBytes @Buffer ((Data.Vector.length (counterBuffers)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (counterBuffers)
  pCounterBufferOffsets <- if Data.Vector.null (counterBufferOffsets)
    then pure nullPtr
    else do
      pPCounterBufferOffsets <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (counterBufferOffsets))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBufferOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((counterBufferOffsets))
      pure $ pPCounterBufferOffsets
  lift $ traceAroundEvent "vkCmdEndTransformFeedbackEXT" (vkCmdEndTransformFeedbackEXT'
                                                            (commandBufferHandle (commandBuffer))
                                                            (firstCounterBuffer)
                                                            ((fromIntegral pCounterBuffersLength :: Word32))
                                                            (pPCounterBuffers)
                                                            pCounterBufferOffsets)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQueryIndexedEXT
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> IO ()

-- | vkCmdBeginQueryIndexedEXT - Begin an indexed query
--
-- = Description
--
-- The 'cmdBeginQueryIndexedEXT' command operates the same as the
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery' command, except that
-- it also accepts a query type specific @index@ parameter.
--
-- This command defines an execution dependency between other query
-- commands that reference the same query index.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @query@ and @index@ that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @query@ and @index@ that occur later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The operation of this command happens after the first scope and happens
-- before the second scope.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-None-00807# All queries used by the
--     command /must/ be /unavailable/
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-02804# The @queryType@
--     used to create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-04728# The @queryType@
--     used to create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-06741# The @queryType@
--     used to create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR'
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-04729# The @queryType@
--     used to create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-00800# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-occlusionQueryPrecise occlusionQueryPrecise>
--     feature is not enabled, or the @queryType@ used to create
--     @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION', @flags@ /must/
--     not contain
--     'Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT'
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-query-00802# @query@ /must/ be less
--     than the number of queries in @queryPool@
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-00803# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION', the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-00804# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS' and
--     any of the @pipelineStatistics@ indicate graphics operations, the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-00805# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS' and
--     any of the @pipelineStatistics@ indicate compute operations, the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-commandBuffer-01885# @commandBuffer@
--     /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-query-00808# If called within a
--     render pass instance, the sum of @query@ and the number of bits set
--     in the current subpass’s view mask /must/ be less than or equal to
--     the number of queries in @queryPool@
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-07126# If the @queryType@
--     used to create @queryPool@ was
--     @VK_QUERY_TYPE_RESULT_STATUS_ONLY_KHR@, then the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ have been created with a queue family index
--     that supports
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-result-status-only result status queries>,
--     as indicated by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFamilyQueryResultStatusPropertiesKHR VkQueueFamilyQueryResultStatusPropertiesKHR>::@queryResultStatusSupport@
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-None-07127# If there is a bound
--     video session, then there /must/ be no
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--     queries
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-07128# If the @queryType@
--     used to create @queryPool@ was
--     @VK_QUERY_TYPE_RESULT_STATUS_ONLY_KHR@ and there is a bound video
--     session, then @queryPool@ /must/ have been created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.Query.QueryPoolCreateInfo' identical to the one
--     specified in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoSessionCreateInfoKHR VkVideoSessionCreateInfoKHR>::@pVideoProfile@
--     the bound video session was created with
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-04862# If the @queryType@
--     used to create @queryPool@ was
--     @VK_QUERY_TYPE_VIDEO_ENCODE_FEEDBACK_KHR@, then the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#video-encode-operations video encode operations>
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-07129# If the @queryType@
--     used to create @queryPool@ was
--     @VK_QUERY_TYPE_VIDEO_ENCODE_FEEDBACK_KHR@, then there /must/ be a
--     bound video session
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-07130# If the @queryType@
--     used to create @queryPool@ was
--     @VK_QUERY_TYPE_VIDEO_ENCODE_FEEDBACK_KHR@ and there is a bound video
--     session, then @queryPool@ /must/ have been created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.Query.QueryPoolCreateInfo' identical to the one
--     specified in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoSessionCreateInfoKHR VkVideoSessionCreateInfoKHR>::@pVideoProfile@
--     the bound video session was created with
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-07131# If the @queryType@
--     used to create @queryPool@ was not
--     @VK_QUERY_TYPE_RESULT_STATUS_ONLY_KHR@ or
--     @VK_QUERY_TYPE_VIDEO_ENCODE_FEEDBACK_KHR@, then there /must/ be no
--     bound video session
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryPool-04753# If the @queryPool@
--     was created with the same @queryType@ as that of another
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>
--     query within @commandBuffer@, then @index@ /must/ not match the
--     index used for the active query
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-02338# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-02339# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ parameter /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-06692# If the @queryType@
--     used to create @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     and not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT',
--     the @index@ /must/ be zero
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-06689# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-06690# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     the @index@ parameter /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-06691# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled, the @index@ parameter /must/ be zero
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-06693# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     then
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-primitivesGeneratedQuery primitivesGeneratedQuery>
--     /must/ be enabled
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-02341# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     then
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackQueries@
--     /must/ be supported
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-07071# The @queryType@
--     used to create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT'
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryPool-07289# If @queryPool@ was
--     created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then the
--     'Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR'::@queueFamilyIndex@
--     @queryPool@ was created with /must/ equal the queue family index of
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryPool-03223# If @queryPool@ was
--     created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#profiling-lock profiling lock>
--     /must/ have been held before
--     'Vulkan.Core10.CommandBuffer.beginCommandBuffer' was called on
--     @commandBuffer@
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryPool-03224# If @queryPool@ was
--     created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     one of the counters used to create @queryPool@ was
--     'Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR',
--     the query begin /must/ be the first recorded command in
--     @commandBuffer@
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryPool-03225# If @queryPool@ was
--     created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     one of the counters used to create @queryPool@ was
--     'Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR',
--     the begin command /must/ not be recorded within a render pass
--     instance
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryPool-03226# If @queryPool@ was
--     created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     another query pool with a @queryType@
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' has
--     been used within @commandBuffer@, its parent primary command buffer
--     or secondary command buffer recorded within the same parent primary
--     command buffer as @commandBuffer@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-performanceCounterMultipleQueryPools performanceCounterMultipleQueryPools>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-None-02863# If @queryPool@ was
--     created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     this command /must/ not be recorded in a command buffer that, either
--     directly or through secondary command buffers, also contains a
--     'Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool' command
--     affecting the same query
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryPool-parameter# @queryPool@
--     /must/ be a valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
--     values
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-commonparent# Both of
--     @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlags',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery',
-- 'cmdEndQueryIndexedEXT'
cmdBeginQueryIndexedEXT :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which this command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @queryPool@ is the query pool that will manage the results of the query.
                           QueryPool
                        -> -- | @query@ is the query index within the query pool that will contain the
                           -- results.
                           ("query" ::: Word32)
                        -> -- | @flags@ is a bitmask of
                           -- 'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
                           -- specifying constraints on the types of queries that /can/ be performed.
                           QueryControlFlags
                        -> -- | @index@ is the query type specific index. When the query type is
                           -- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
                           -- or 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT',
                           -- the index represents the vertex stream.
                           ("index" ::: Word32)
                        -> io ()
cmdBeginQueryIndexedEXT commandBuffer queryPool query flags index = liftIO $ do
  let vkCmdBeginQueryIndexedEXTPtr = pVkCmdBeginQueryIndexedEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdBeginQueryIndexedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginQueryIndexedEXT is null" Nothing Nothing
  let vkCmdBeginQueryIndexedEXT' = mkVkCmdBeginQueryIndexedEXT vkCmdBeginQueryIndexedEXTPtr
  traceAroundEvent "vkCmdBeginQueryIndexedEXT" (vkCmdBeginQueryIndexedEXT'
                                                  (commandBufferHandle (commandBuffer))
                                                  (queryPool)
                                                  (query)
                                                  (flags)
                                                  (index))
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginQueryIndexedEXT' and 'cmdEndQueryIndexedEXT'
--
-- Note that 'cmdEndQueryIndexedEXT' is *not* called if an exception is
-- thrown by the inner action.
cmdUseQueryIndexedEXT :: forall io r . MonadIO io => CommandBuffer -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> io r -> io r
cmdUseQueryIndexedEXT commandBuffer queryPool query flags index a =
  (cmdBeginQueryIndexedEXT commandBuffer
                             queryPool
                             query
                             flags
                             index) *> a <* (cmdEndQueryIndexedEXT commandBuffer
                                                                     queryPool
                                                                     query
                                                                     index)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQueryIndexedEXT
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- | vkCmdEndQueryIndexedEXT - Ends a query
--
-- = Description
--
-- The command completes the query in @queryPool@ identified by @query@ and
-- @index@, and marks it as available.
--
-- The 'cmdEndQueryIndexedEXT' command operates the same as the
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery' command, except that
-- it also accepts a query type specific @index@ parameter.
--
-- This command defines an execution dependency between other query
-- commands that reference the same query index.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @query@ that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the operation of this command.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-None-02342# All queries used by the
--     command /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-query-02343# @query@ /must/ be less
--     than the number of queries in @queryPool@
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-commandBuffer-02344# @commandBuffer@
--     /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-query-02345# If
--     'cmdEndQueryIndexedEXT' is called within a render pass instance, the
--     sum of @query@ and the number of bits set in the current subpass’s
--     view mask /must/ be less than or equal to the number of queries in
--     @queryPool@
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-queryType-06694# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT',
--     the @index@ parameter /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-queryType-06695# If the @queryType@
--     used to create @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     and not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT',
--     the @index@ /must/ be zero
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-queryType-06696# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT',
--     @index@ /must/ equal the @index@ used to begin the query
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-None-07007# If called within a subpass
--     of a render pass instance, the corresponding
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery'* command /must/
--     have been called previously within the same subpass
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-None-07008# If called outside of a
--     render pass instance, the corresponding
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery'* command /must/
--     have been called outside of a render pass instance
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-queryPool-parameter# @queryPool@
--     /must/ be a valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-commonparent# Both of @commandBuffer@,
--     and @queryPool@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery',
-- 'cmdBeginQueryIndexedEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery'
cmdEndQueryIndexedEXT :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which this command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @queryPool@ is the query pool that is managing the results of the query.
                         QueryPool
                      -> -- | @query@ is the query index within the query pool where the result is
                         -- stored.
                         ("query" ::: Word32)
                      -> -- | @index@ is the query type specific index.
                         ("index" ::: Word32)
                      -> io ()
cmdEndQueryIndexedEXT commandBuffer queryPool query index = liftIO $ do
  let vkCmdEndQueryIndexedEXTPtr = pVkCmdEndQueryIndexedEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdEndQueryIndexedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndQueryIndexedEXT is null" Nothing Nothing
  let vkCmdEndQueryIndexedEXT' = mkVkCmdEndQueryIndexedEXT vkCmdEndQueryIndexedEXTPtr
  traceAroundEvent "vkCmdEndQueryIndexedEXT" (vkCmdEndQueryIndexedEXT'
                                                (commandBufferHandle (commandBuffer))
                                                (queryPool)
                                                (query)
                                                (index))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectByteCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawIndirectByteCountEXT - Draw primitives with indirect parameters
-- where the vertex count is derived from the counter byte value in the
-- counter buffer
--
-- = Description
--
-- When the command is executed, primitives are assembled in the same way
-- as done with 'Vulkan.Core10.CommandBufferBuilding.cmdDraw' except the
-- @vertexCount@ is calculated based on the byte count read from
-- @counterBuffer@ at offset @counterBufferOffset@. The assembled
-- primitives execute the bound graphics pipeline.
--
-- The effective @vertexCount@ is calculated as follows:
--
-- > const uint32_t * counterBufferPtr = (const uint8_t *)counterBuffer.address + counterBufferOffset;
-- > vertexCount = floor(max(0, (*counterBufferPtr - counterOffset)) / vertexStride);
--
-- The effective @firstVertex@ is zero.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-cubicRangeClamp-09212# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-filter-cubic-range-clamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-selectableCubicWeights-09214# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-filter-cubic-weight-selection selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08600# For each set /n/
--     that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a descriptor set /must/ have been bound to /n/ at the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for set /n/, with the
--     'Vulkan.Core10.Handles.PipelineLayout' or
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array that was used to
--     create the current 'Vulkan.Core10.Handles.Pipeline' or
--     'Vulkan.Extensions.Handles.ShaderEXT', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08601# For each push
--     constant that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' or
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Core10.Handles.Pipeline' or
--     'Vulkan.Extensions.Handles.ShaderEXT', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-maintenance4-08602# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' or
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Core10.Handles.Pipeline' or
--     'Vulkan.Extensions.Handles.ShaderEXT', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08114# Descriptors in each
--     bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08115# If the descriptors
--     used by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08116# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08604# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08117# If the descriptors
--     used by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08119# If a descriptor is
--     dynamically used with a 'Vulkan.Core10.Handles.Pipeline' created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08605# If a descriptor is
--     dynamically used with a 'Vulkan.Extensions.Handles.ShaderEXT'
--     created with a 'Vulkan.Core10.Handles.DescriptorSetLayout' that was
--     created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08607# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     is enabled, either a valid pipeline /must/ be bound to the pipeline
--     bind point used by this command, or a valid combination of valid and
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' shader objects /must/ be
--     bound to every supported shader stage corresponding to the pipeline
--     bind point used by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08608# If a pipeline is
--     bound to the pipeline bind point used by this command, there /must/
--     not have been any calls to dynamic state setting commands for any
--     state not specified as dynamic in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command, since that pipeline was bound
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08609# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08610# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08611# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-uniformBuffers-06935# If any
--     stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     and that stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-storageBuffers-06936# If any
--     stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     and that stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-06550# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-ConstOffset-06551# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-operation-validation ???>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the image view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sparseImageInt64Atomics-04474#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sparseImageInt64Atomics-04475#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageWeightedSampleQCOM-06971#
--     If @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageWeightedSampleQCOM-06972#
--     If @OpImageWeightedSampleQCOM@ uses a
--     'Vulkan.Core10.Handles.ImageView' as a sample weight image as a
--     result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageBlockMatchSSDQCOM-06974#
--     If @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageBlockMatchSADQCOM-06975#
--     If @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageBlockMatchSADQCOM-06976#
--     If @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageWeightedSampleQCOM-06977#
--     If @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageWeightedSampleQCOM-06978#
--     If any command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageBlockMatchWindow-09215#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageBlockMatchWindow-09216#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format.
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageBlockMatchWindow-09217#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ read from a reference image as result
--     of this command, then the specified reference coordinates /must/ not
--     fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07288# Any shader
--     invocation executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-renderPass-02684# The current
--     render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-subpass-02685# The subpass index
--     of the current render pass /must/ be equal to the @subpass@ member
--     of the 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07748# If any shader
--     statically accesses an input attachment, a valid descriptor /must/
--     be bound to the pipeline via a descriptor set
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpTypeImage-07468# If any shader
--     executed by this pipeline accesses an @OpTypeImage@ variable with a
--     @Dim@ operand of @SubpassData@, it /must/ be decorated with an
--     @InputAttachmentIndex@ that corresponds to a valid input attachment
--     in the current subpass
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07469# Input attachment
--     views accessed in a subpass /must/ be created with the same
--     'Vulkan.Core10.Enums.Format.Format' as the corresponding subpass
--     definition, and be created with a 'Vulkan.Core10.Handles.ImageView'
--     that is compatible with the attachment referenced by the subpass\'
--     @pInputAttachments@[@InputAttachmentIndex@] in the currently bound
--     'Vulkan.Core10.Handles.Framebuffer' as specified by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#compatibility-inputattachment Fragment Input Attachment Compatibility>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-06537# Memory backing image
--     subresources used as attachments in the current render pass /must/
--     not be written in any way other than as an attachment by this
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09000# If a color
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, it is not in
--     the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     image layout, and either:
--
--     -   the
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--         is set on the currently bound pipeline or
--
--     -   the last call to
--         'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--         included
--         'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--         and
--
--         -   there is no currently bound graphics pipeline or
--
--         -   the currently bound graphics pipeline was created with
--             'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--             it /must/ not be accessed in any way other than as an
--             attachment by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09001# If a depth
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, it is not in
--     the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     image layout, and either:
--
--     -   the
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--         is set on the currently bound pipeline or
--
--     -   the last call to
--         'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--         included
--         'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--         and
--
--         -   there is no currently bound graphics pipeline or
--
--         -   the currently bound graphics pipeline was created with
--             'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--             it /must/ not be accessed in any way other than as an
--             attachment by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09002# If a stencil
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, it is not in
--     the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     image layout, and either:
--
--     -   the
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--         is set on the currently bound pipeline or
--
--     -   the last call to
--         'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--         included
--         'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--         and
--
--         -   there is no currently bound graphics pipeline or
--
--         -   the currently bound graphics pipeline was created with
--             'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--             it /must/ not be accessed in any way other than as an
--             attachment by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09003# If an attachment is
--     written by any prior command in this subpass or by the load, store,
--     or resolve operations for this subpass, it /must/ not be accessed in
--     any way other than as an attachment, storage image, or sampled image
--     by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-06539# If any previously
--     recorded command in the current subpass accessed an image
--     subresource used as an attachment in this subpass in any way other
--     than as an attachment, this command /must/ not write to that image
--     subresource as an attachment
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-06886# If the current
--     render pass instance uses a depth\/stencil attachment with a
--     read-only layout for the depth aspect,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth-write depth writes>
--     /must/ be disabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-06887# If the current
--     render pass instance uses a depth\/stencil attachment with a
--     read-only layout for the stencil aspect, both front and back
--     @writeMask@ are not zero, and stencil test is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil all stencil ops>
--     /must/ be 'Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07831# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' /must/ have
--     been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07832# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetScissor' /must/ have been
--     called in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07833# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' /must/ have
--     been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08617# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     in the current command buffer set @polygonMode@ to
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_LINE',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' /must/ have
--     been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08618# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     in the current command buffer set @primitiveTopology@ to any line
--     topology, 'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08619# If a shader object
--     that outputs line primitives is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' /must/ have
--     been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07834# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias' or
--     'Vulkan.Extensions.VK_EXT_depth_bias_control.cmdSetDepthBias2EXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08620# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
--     in the current command buffer set @depthBiasEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias' or
--     'Vulkan.Extensions.VK_EXT_depth_bias_control.cmdSetDepthBias2EXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07835# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_BLEND_CONSTANTS'
--     dynamic state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants' /must/
--     have been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08621# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     in the current command buffer set any element of
--     @pColorBlendEnables@ to 'Vulkan.Core10.FundamentalTypes.TRUE', and
--     the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     in the current command buffer set the same element of
--     @pColorBlendEquations@ to a
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.ColorBlendEquationEXT'
--     structure with any 'Vulkan.Core10.Enums.BlendFactor.BlendFactor'
--     member with a value of
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants' /must/
--     have been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07836# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS'
--     dynamic state enabled, and if the current @depthBoundsTestEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBounds' /must/ have
--     been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08622# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnable'
--     in the current command buffer set @depthBoundsTestEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBounds' /must/ have
--     been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07837# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_COMPARE_MASK'
--     dynamic state enabled, and if the current @stencilTestEnable@ state
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilCompareMask'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08623# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable'
--     in the current command buffer set @stencilTestEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilCompareMask'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07838# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_WRITE_MASK'
--     dynamic state enabled, and if the current @stencilTestEnable@ state
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask' /must/
--     have been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08624# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable'
--     in the current command buffer set @stencilTestEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask' /must/
--     have been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07839# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_REFERENCE'
--     dynamic state enabled, and if the current @stencilTestEnable@ state
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilReference' /must/
--     have been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08625# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable'
--     in the current command buffer set @stencilTestEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilReference' /must/
--     have been called in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-maxMultiviewInstanceIndex-02688#
--     If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsEnable-02689# If
--     the bound graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-06666# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08626# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT'
--     in the current command buffer set @sampleLocationsEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsPerPixel-07934#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled, then the @sampleLocationsPerPixel@ member of
--     @pSampleLocationsInfo@ in the last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ equal the @rasterizationSamples@ member of the
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'
--     structure the bound graphics pipeline has been created with
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07840# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE' dynamic
--     state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetCullMode'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08627# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetCullMode'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07841# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE' dynamic
--     state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetFrontFace'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08628# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetFrontFace'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07843# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthTestEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08629# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthTestEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07844# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08630# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07845# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthCompareOp'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08631# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthTestEnable'
--     in the current command buffer set @depthTestEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthCompareOp'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07846# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08632# If a shader object
--     is bound to any graphics stage, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthBounds depthBounds>
--     feature is enabled, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then the
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07847# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08633# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07848# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP' dynamic
--     state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilOp'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08634# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable'
--     in the current command buffer set @stencilTestEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilOp'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-03417# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-scissorCount-03418# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-03419# If the
--     bound graphics pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic states enabled then both
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     and
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08635# If a shader object
--     is bound to any graphics stage, then both
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     and
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-04137# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-04138# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09232# If the
--     @VK_NV_clip_space_w_scaling@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportWScalingEnableNV'
--     in the current command buffer set @viewportWScalingEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08636# If the
--     @VK_NV_clip_space_w_scaling@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportWScalingEnableNV'
--     in the current command buffer set @viewportWScalingEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the @viewportCount@
--     parameter in the last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-04139# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-04140# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-shadingRateImage-09233# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetCoarseSampleOrderNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-shadingRateImage-09234# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetShadingRateImageEnableNV'
--     in the current command buffer set @shadingRateImageEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08637# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetShadingRateImageEnableNV'
--     in the current command buffer set @shadingRateImageEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the @viewportCount@
--     parameter in the last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-VkPipelineVieportCreateInfo-04141#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-VkPipelineVieportCreateInfo-04142#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07878# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_ENABLE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07879# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-exclusiveScissor-09235# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-exclusiveScissor exclusiveScissor>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, then
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08638# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-exclusiveScissor exclusiveScissor>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorEnableNV'
--     in the current command buffer set any element of
--     @pExclusiveScissorEnables@ to 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04876# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08639# If a shader object
--     is bound to any graphics stage, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04877# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08640# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-logicOp-04878# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command and the @logicOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08641# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLogicOpEnableEXT'
--     set @logicOpEnable@ to 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command and the @logicOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-primitiveFragmentShadingRateWithMultipleViewports-08642#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, and any shader object bound to a graphics
--     stage writes to the @PrimitiveShadingRateKHR@ built-in, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-blendEnable-04727# If
--     rasterization is not disabled in the bound graphics pipeline, then
--     for each color attachment in the subpass, if the corresponding image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the @blendEnable@ member of the corresponding element of the
--     @pAttachments@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08643# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then for each color
--     attachment in the render pass, if the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the corresponding member of @pColorBlendEnables@ in the most
--     recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     in the current command buffer that affected that attachment index
--     /must/ have been 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-multisampledRenderToSingleSampled-07284#
--     If rasterization is not disabled in the bound graphics pipeline, and
--     none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is enabled, then @rasterizationSamples@ for the currently
--     bound graphics pipeline /must/ be the same as the current subpass
--     color and\/or depth\/stencil attachments
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08644# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and none of the
--     @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is enabled, then the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     in the current command buffer /must/ have set @rasterizationSamples@
--     to be the same as the number of samples for the current render pass
--     color and\/or depth\/stencil attachments
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08876# If a shader object
--     is bound to any graphics stage, the current render pass instance
--     /must/ have been begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-imageView-06172# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-imageView-06173# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-imageView-06174# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-imageView-06175# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-imageView-06176# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-imageView-06177# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewMask-06178# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-colorAttachmentCount-06179# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08910#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08911#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled, and the current render pass instance was begun
--     with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the currently bound graphics pipeline, or the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@,
--     if it exists, /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08912#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the currently bound pipeline equal to
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-colorAttachmentCount-09362# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, there is no shader object bound to any graphics stage,
--     and a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID',
--     each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @resolveImageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with an image created with a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value equal to the
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09363# If there is no
--     shader object bound to any graphics stage, the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, and a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID',
--     each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with an image created with a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value equal to the
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09364# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, and the
--     currently bound graphics pipeline was created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value and with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have set the blend enable to
--     'Vulkan.Core10.FundamentalTypes.FALSE' prior to this drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09365# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, and the
--     currently bound graphics pipeline was created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value and with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ have set @rasterizationSamples@ to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' prior
--     to this drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09366# If there is a shader
--     object bound to any graphics stage, and the current render pass
--     includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have set blend enable to
--     'Vulkan.Core10.FundamentalTypes.FALSE' prior to this drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizationSamples-09367# If
--     there is a shader object bound to any graphics stage, and the
--     current render pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ have set @rasterizationSamples@ to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' prior
--     to this drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09368# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, and the
--     currently bound graphics pipeline was created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value and with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->width@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09369# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, and the
--     currently bound graphics pipeline was created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value and with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->height@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pFragmentSize-09370# If there is
--     a shader object bound to any graphics stage, and the current render
--     pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->width@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pFragmentSize-09371# If there is
--     a shader object bound to any graphics stage, and the current render
--     pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->height@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07749# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08646# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature is enabled on the device, and a shader object is bound to
--     the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-attachmentCount-07750# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be greater than or equal to the
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@attachmentCount@
--     of the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08647# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature is enabled on the device, and a shader object is bound to
--     the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then the @attachmentCount@
--     parameter of most recent call to
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     in the current command buffer /must/ be greater than or equal to the
--     number of color attachments in the current render pass instance
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07751# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command for each discard rectangle in
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'::@discardRectangleCount@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07880# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizerDiscardEnable-09236#
--     If the @VK_EXT_discard_rectangles@ extension is enabled, and a
--     shader object is bound to any graphics stage, and the most recent
--     call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEnableEXT'
--     in the current command buffer set @discardRectangleEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08648# If the
--     @VK_EXT_discard_rectangles@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07881# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_MODE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08649# If the
--     @VK_EXT_discard_rectangles@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEnableEXT'
--     in the current command buffer set @discardRectangleEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08913#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08914#
--     If current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08915#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the currently bound graphics pipeline was not equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@,
--     the value of the format /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08916#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08917#
--     If current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicRenderingUnusedAttachments-08918#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the currently bound graphics pipeline was not equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@,
--     the value of the format /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-imageView-06183# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-imageView-06184# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-colorAttachmentCount-06185# If
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the corresponding element of the
--     @pColorAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pDepthAttachment-06186# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pStencilAttachment-06187# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-multisampledRenderToSingleSampled-07285#
--     If the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the value of @rasterizationSamples@ for
--     the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-multisampledRenderToSingleSampled-07286#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     @rasterizationSamples@ for the currently bound graphics pipeline
--     /must/ be equal to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-multisampledRenderToSingleSampled-07287#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     @rasterizationSamples@ for the currently bound graphics pipeline
--     /must/ be equal to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pNext-07935# If this command has
--     been called inside a render pass instance started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and the @pNext@ chain of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
--     includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with @multisampledRenderToSingleSampledEnable@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the value of
--     @rasterizationSamples@ for the currently bound graphics pipeline
--     /must/ be equal to
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'::@rasterizationSamples@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-renderPass-06198# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline /must/ have been created with a
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@renderPass@
--     equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pColorAttachments-08963# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is a graphics pipeline bound with a fragment shader that
--     statically writes to a color attachment, the color write mask is not
--     zero, color writes are enabled, and the corresponding element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pDepthAttachment-08964# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is a graphics pipeline bound, depth test is enabled, depth
--     write is enabled, and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pStencilAttachment-08965# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is a graphics pipeline bound, stencil test is enabled and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-primitivesGeneratedQueryWithRasterizerDiscard-06708#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithRasterizerDiscard primitivesGeneratedQueryWithRasterizerDiscard>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-discard rasterization discard>
--     /must/ not be enabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-primitivesGeneratedQueryWithNonZeroStreams-06709#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, the bound graphics pipeline /must/ not have been
--     created with a non-zero value in
--     'PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07619# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_TESSELLATION_DOMAIN_ORIGIN_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetTessellationDomainOriginEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07620# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClampEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09237# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetTessellationDomainOriginEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08650# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClamp depthClamp>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClampEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07621# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_POLYGON_MODE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08651# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07622# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08652# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07623# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08653# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07624# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-alphaToCoverageEnable-08919# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--     dynamic state enabled, and @alphaToCoverageEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-fragmentoutput Fragment Output Interface>
--     /must/ contain a variable for the alpha @Component@ word in
--     @Location@ 0 at @Index@ 0
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08654# If a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-alphaToCoverageEnable-08920# If
--     a shader object is bound to any graphics stage, and the most recent
--     call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT'
--     in the current command buffer set @alphaToCoverageEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-fragmentoutput Fragment Output Interface>
--     /must/ contain a variable for the alpha @Component@ word in
--     @Location@ 0 at @Index@ 0
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07625# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToOneEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08655# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-alphaToOne alphaToOne>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToOneEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07626# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLogicOpEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08656# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-logicOp logicOp>
--     feature is enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLogicOpEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07627# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08657# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07628# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08658# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     for any attachment set that attachment’s value in
--     @pColorBlendEnables@ to 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07629# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08659# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07630# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationStreamEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08660# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryStreams geometryStreams>
--     feature is enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationStreamEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07631# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetConservativeRasterizationModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08661# If the
--     @VK_EXT_conservative_rasterization@ extension is enabled, and a
--     shader object is bound to any graphics stage, and the most recent
--     call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetConservativeRasterizationModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07632# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXTRA_PRIMITIVE_OVERESTIMATION_SIZE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetExtraPrimitiveOverestimationSizeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08662# If the
--     @VK_EXT_conservative_rasterization@ extension is enabled, and a
--     shader object is bound to any graphics stage, and the most recent
--     call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetConservativeRasterizationModeEXT'
--     in the current command buffer set @conservativeRasterizationMode@ to
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT',
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetExtraPrimitiveOverestimationSizeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07633# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08663# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClipEnable depthClipEnable>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07634# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08664# If the
--     @VK_EXT_sample_locations@ extension is enabled, and a shader object
--     is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07635# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizerDiscardEnable-09416#
--     If the @VK_EXT_blend_operation_advanced@ extension is enabled, and a
--     shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then at least one of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07636# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PROVOKING_VERTEX_MODE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetProvokingVertexModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08665# If the
--     @VK_EXT_provoking_vertex@ extension is enabled, and a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetProvokingVertexModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07637# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08666# If the
--     @VK_EXT_line_rasterization@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     in the current command buffer set @polygonMode@ to
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_LINE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08667# If the
--     @VK_EXT_line_rasterization@ extension is enabled, and a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     in the current command buffer set @primitiveTopology@ to any line
--     topology, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08668# If the
--     @VK_EXT_line_rasterization@ extension is enabled, and a shader
--     object that outputs line primitives is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07638# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08669# If the
--     @VK_EXT_line_rasterization@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     in the current command buffer set @polygonMode@ to
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_LINE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08670# If the
--     @VK_EXT_line_rasterization@ extension is enabled, and a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     in the current command buffer set @primitiveTopology@ to any line
--     topology, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08671# If the
--     @VK_EXT_line_rasterization@ extension is enabled, and a shader
--     object that outputs line primitives is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07849# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_line_rasterization.cmdSetLineStippleEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08672# If the
--     @VK_EXT_line_rasterization@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     in the current command buffer set @stippledLineEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_line_rasterization.cmdSetLineStippleEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07639# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_NEGATIVE_ONE_TO_ONE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipNegativeOneToOneEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08673# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClipControl depthClipControl>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipNegativeOneToOneEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07640# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_ENABLE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportWScalingEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08674# If the
--     @VK_NV_clip_space_w_scaling@ extension is enabled, and a shader
--     object is bound to any graphics stage, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportWScalingEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07641# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08675# If the
--     @VK_NV_viewport_swizzle@ extension is enabled, and a shader object
--     is bound to any graphics stage, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07642# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08676# If the
--     @VK_NV_fragment_coverage_to_color@ extension is enabled, and a
--     shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07643# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_LOCATION_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorLocationNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08677# If the
--     @VK_NV_fragment_coverage_to_color@ extension is enabled, and a
--     shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     in the current command buffer set @coverageToColorEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorLocationNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07644# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_MODE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationModeNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08678# If the
--     @VK_NV_framebuffer_mixed_samples@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationModeNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07645# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08679# If the
--     @VK_NV_framebuffer_mixed_samples@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationModeNV'
--     in the current command buffer set coverageModulationMode to any
--     value other than
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.COVERAGE_MODULATION_MODE_NONE_NV',
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07646# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08680# If the
--     @VK_NV_framebuffer_mixed_samples@ extension is enabled, and a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableEnableNV'
--     in the current command buffer set @coverageModulationTableEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07647# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SHADING_RATE_IMAGE_ENABLE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetShadingRateImageEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pipelineFragmentShadingRate-09238#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     feature is enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set rasterizerDiscardEnable to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08681# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetShadingRateImageEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07648# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_REPRESENTATIVE_FRAGMENT_TEST_ENABLE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRepresentativeFragmentTestEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08682# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-representativeFragmentTest representativeFragmentTest>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRepresentativeFragmentTestEnableNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07649# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_REDUCTION_MODE_NV'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageReductionModeNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08683# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-coverageReductionMode coverageReductionMode>
--     feature is enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageReductionModeNV'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pColorBlendEnables-07470# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     state enabled and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     set @pColorBlendEnables@ for any attachment to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then for those attachments in
--     the subpass the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizationSamples-07471# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the current subpass does not use any color
--     and\/or depth\/stencil attachments, then the @rasterizationSamples@
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ follow the rules for a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-noattachments zero-attachment subpass>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-samples-07472# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state disabled, then the @samples@ parameter in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     /must/ be greater or equal to the
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     parameter used to create the bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-samples-07473# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--     state and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     states enabled, then the @samples@ parameter in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     /must/ be greater or equal to the @rasterizationSamples@ parameter
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizationSamples-07474# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and neither the @VK_AMD_mixed_attachment_samples@ nor
--     the @VK_NV_framebuffer_mixed_samples@ extensions are enabled, then
--     the @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ be the same as the current subpass color and\/or
--     depth\/stencil attachments
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09211# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, or a shader object is bound to any graphics stage,
--     and the current render pass instance includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with @multisampledRenderToSingleSampledEnable@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ be the same as the @rasterizationSamples@ member of that
--     structure
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-firstAttachment-07476# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     calls /must/ specify an enable for all active color attachments in
--     the current subpass
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizerDiscardEnable-09417#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     calls /must/ specify an enable for all active color attachments in
--     the current subpass
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-firstAttachment-07477# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     calls /must/ specify the blend equations for all active color
--     attachments in the current subpass where blending is enabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizerDiscardEnable-09418#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     calls /must/ specify the blend equations for all active color
--     attachments in the current subpass where blending is enabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-firstAttachment-07478# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     calls /must/ specify the color write mask for all active color
--     attachments in the current subpass
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizerDiscardEnable-09419#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     calls /must/ specify the color write mask for all active color
--     attachments in the current subpass
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-firstAttachment-07479# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     calls /must/ specify the advanced blend equations for all active
--     color attachments in the current subpass where blending is enabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-advancedBlendMaxColorAttachments-07480#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic states enabled and the last calls to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     have enabled advanced blending, then the number of active color
--     attachments in the current subpass /must/ not exceed
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendMaxColorAttachments advancedBlendMaxColorAttachments>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-primitivesGeneratedQueryWithNonZeroStreams-07481#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, and the bound graphics pipeline was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
--     state enabled, the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationStreamEXT'
--     /must/ have set the @rasterizationStream@ to zero
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsPerPixel-07482#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state disabled, then the @sampleLocationsPerPixel@ member of
--     @pSampleLocationsInfo@ in the last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ equal the @rasterizationSamples@ member of the
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'
--     structure the bound graphics pipeline has been created with
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsPerPixel-07483#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, then the @sampleLocationsPerPixel@ member of
--     @pSampleLocationsInfo@ in the last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ equal the @rasterizationSamples@ parameter of the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsEnable-07484# If
--     a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     and the current subpass has a depth\/stencil attachment, then that
--     attachment /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsEnable-07485# If
--     a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and if @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     then the @sampleLocationsInfo.sampleLocationGridSize.width@ in the
--     last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.width@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsEnable-07486# If
--     a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and if @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     then the @sampleLocationsInfo.sampleLocationGridSize.height@ in the
--     last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.height@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsEnable-07487# If
--     a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and if @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     the fragment shader code /must/ not statically use the extended
--     instruction @InterpolateAtSample@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsEnable-07936# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state disabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, the @sampleLocationsEnable@ member of a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     in the bound graphics pipeline is
--     'Vulkan.Core10.FundamentalTypes.TRUE' or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, then,
--     @sampleLocationsInfo.sampleLocationGridSize.width@ /must/ evenly
--     divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.width@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling the value of
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsEnable-07937# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state disabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, the @sampleLocationsEnable@ member of a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     in the bound graphics pipeline is
--     'Vulkan.Core10.FundamentalTypes.TRUE' or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, then,
--     @sampleLocationsInfo.sampleLocationGridSize.height@ /must/ evenly
--     divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.height@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling the value of
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sampleLocationsEnable-07938# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state disabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, the @sampleLocationsEnable@ member of a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     in the bound graphics pipeline is
--     'Vulkan.Core10.FundamentalTypes.TRUE' or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, then, @sampleLocationsInfo.sampleLocationsPerPixel@
--     /must/ equal @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-coverageModulationTableEnable-07488#
--     If a shader object is bound to any graphics stage or the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
--     state enabled, and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableEnableNV'
--     set @coverageModulationTableEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     @coverageModulationTableCount@ parameter in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableNV'
--     /must/ equal the current @rasterizationSamples@ divided by the
--     number of color samples in the current subpass
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizationSamples-07489# If
--     the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and if
--     current subpass has a depth\/stencil attachment and depth test,
--     stencil test, or depth bounds test are enabled in the currently
--     bound pipeline state, then the current @rasterizationSamples@ /must/
--     be the same as the sample count of the depth\/stencil attachment
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-coverageToColorEnable-07490# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--     state enabled and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     set the @coverageToColorEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the current subpass
--     /must/ have a color attachment at the location selected by the last
--     call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorLocationNV'
--     @coverageToColorLocation@, with a
--     'Vulkan.Core10.Enums.Format.Format' of
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_UINT', or
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_SINT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizerDiscardEnable-09420#
--     If the @VK_NV_fragment_coverage_to_color@ extension is enabled, and
--     a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     set the @coverageToColorEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the current subpass
--     /must/ have a color attachment at the location selected by the last
--     call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorLocationNV'
--     @coverageToColorLocation@, with a
--     'Vulkan.Core10.Enums.Format.Format' of
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_UINT', or
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_SINT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-coverageReductionMode-07491# If
--     this @VK_NV_coverage_reduction_mode@ extension is enabled, the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     states enabled, the current coverage reduction mode
--     @coverageReductionMode@, then the current @rasterizationSamples@,
--     and the sample counts for the color and depth\/stencil attachments
--     (if the subpass has them) /must/ be a valid combination returned by
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-07492# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-07493# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-09421# If the
--     @VK_NV_viewport_swizzle@ extension is enabled, and a shader object
--     is bound to any graphics stage, then the @viewportCount@ parameter
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizationSamples-07494# If
--     the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and if
--     the current subpass has any color attachments and
--     @rasterizationSamples@ of the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     is greater than the number of color samples, then the pipeline
--     @sampleShadingEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-stippledLineEnable-07495# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_RECTANGULAR_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-stippledLineEnable-07496# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_BRESENHAM_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-stippledLineEnable-07497# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-stippledLineEnable-07498# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_DEFAULT_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-conservativePointAndLineRasterization-07499#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
--     dynamic state enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-conservativePointAndLineRasterization conservativePointAndLineRasterization>
--     is not supported, and the effective primitive topology output by the
--     last pre-rasterization shader stage is a line or point, then the
--     @conservativeRasterizationMode@ set by the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetConservativeRasterizationModeEXT'
--     /must/ be
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-stage-07073# If the currently
--     bound pipeline was created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-mesh-shader Mesh Shader Queries>
--     /must/ not be active
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08877# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--     dynamic state
--     'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07850# If dynamic state was
--     inherited from
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV',
--     it /must/ be set in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08684# If there is no bound
--     graphics pipeline,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08685# If there is no bound
--     graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08686# If there is no bound
--     graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08687# If there is no bound
--     graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08688# If there is no bound
--     graphics pipeline,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08689# If there is no bound
--     graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08690# If there is no bound
--     graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08693# If there is no bound
--     graphics pipeline, and at least one of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     features is enabled, one of the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stages /must/ have a valid 'Vulkan.Extensions.Handles.ShaderEXT'
--     bound, and the other /must/ have no
--     'Vulkan.Extensions.Handles.ShaderEXT' bound
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08694# If there is no bound
--     graphics pipeline, and both the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     features are enabled, and a valid
--     'Vulkan.Extensions.Handles.ShaderEXT' is bound the to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stage, and that 'Vulkan.Extensions.Handles.ShaderEXT' was created
--     without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_NO_TASK_SHADER_BIT_EXT'
--     flag, a valid 'Vulkan.Extensions.Handles.ShaderEXT' /must/ be bound
--     to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     stage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08695# If there is no bound
--     graphics pipeline, and both the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     features are enabled, and a valid
--     'Vulkan.Extensions.Handles.ShaderEXT' is bound the to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stage, and that 'Vulkan.Extensions.Handles.ShaderEXT' was created
--     with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_NO_TASK_SHADER_BIT_EXT'
--     flag, there /must/ be no 'Vulkan.Extensions.Handles.ShaderEXT' bound
--     to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     stage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08696# If there is no bound
--     graphics pipeline, and a valid 'Vulkan.Extensions.Handles.ShaderEXT'
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage, there /must/ be no 'Vulkan.Extensions.Handles.ShaderEXT'
--     bound to either the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     stage or the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08698# If any graphics
--     shader is bound which was created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag, then all shaders created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag in the same
--     'Vulkan.Extensions.VK_EXT_shader_object.createShadersEXT' call
--     /must/ also be bound
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08699# If any graphics
--     shader is bound which was created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag, any stages in between stages whose shaders which did not
--     create a shader with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag as part of the same
--     'Vulkan.Extensions.VK_EXT_shader_object.createShadersEXT' call
--     /must/ not have any 'Vulkan.Extensions.Handles.ShaderEXT' bound
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08878# All bound graphics
--     shader objects /must/ have been created with identical or
--     identically defined push constant ranges
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08879# All bound graphics
--     shader objects /must/ have been created with identical or
--     identically defined arrays of descriptor set layouts
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-colorAttachmentCount-09372# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID',
--     and a fragment shader is bound, it /must/ not declare the
--     @DepthReplacing@ or @StencilRefReplacingEXT@ execution modes
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08880# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFeedbackLoopDynamicState attachmentFeedbackLoopDynamicState>
--     feature is enabled on the device, and a shader object is bound to
--     the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pDynamicStates-08715# If the
--     bound graphics pipeline state includes a fragment shader stage, was
--     created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE'
--     set in
--     'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@,
--     and the fragment shader declares the @EarlyFragmentTests@ execution
--     mode and uses @OpDepthAttachmentReadEXT@, the @depthWriteEnable@
--     parameter in the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable'
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pDynamicStates-08716# If the
--     bound graphics pipeline state includes a fragment shader stage, was
--     created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_WRITE_MASK'
--     set in
--     'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@,
--     and the fragment shader declares the @EarlyFragmentTests@ execution
--     mode and uses @OpStencilAttachmentReadEXT@, the @writeMask@
--     parameter in the last call to
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask' /must/
--     be @0@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09116# If a shader object
--     is bound to any graphics stage or the currently bound graphics
--     pipeline was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT',
--     and the format of any color attachment is
--     'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32', the
--     corresponding element of the @pColorWriteMasks@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ either include all of
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_R_BIT',
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_G_BIT',
--     and
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_B_BIT',
--     or none of them
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-maxFragmentDualSrcAttachments-09239#
--     If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blending blending>
--     is enabled for any attachment where either the source or destination
--     blend factors for that attachment
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-dsb use the secondary color input>,
--     the maximum value of @Location@ for any output attachment
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-staticuse statically used>
--     in the @Fragment@ @Execution@ @Model@ executed by this command
--     /must/ be less than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentDualSrcAttachments maxFragmentDualSrcAttachments>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04007# All vertex input
--     bindings accessed via vertex input variables declared in the vertex
--     shader entry point’s interface /must/ have either valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' buffers bound
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04008# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02721# For a given vertex
--     buffer binding, any attribute data fetched /must/ be entirely
--     contained within the corresponding vertex buffer binding, as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-07842# If there is a shader
--     object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-dynamicPrimitiveTopologyUnrestricted-07500#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
--     dynamic state enabled and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-dynamicPrimitiveTopologyUnrestricted dynamicPrimitiveTopologyUnrestricted>
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then the
--     @primitiveTopology@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04912# If the bound
--     graphics pipeline was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic states enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pStrides-04913# If the bound
--     graphics pipeline was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @pStrides@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ not be @NULL@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04914# If there is a shader
--     object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-Input-07939# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled then all variables with the @Input@ storage
--     class decorated with @Location@ in the @Vertex@ @Execution@ @Model@
--     @OpEntryPoint@ /must/ contain a location in
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@location@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-Input-08734# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled then the numeric type associated with all
--     @Input@ variables of the corresponding @Location@ in the @Vertex@
--     @Execution@ @Model@ @OpEntryPoint@ /must/ be the same as
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-format-08936# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled and
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--     has a 64-bit component, then the scalar width associated with all
--     @Input@ variables of the corresponding @Location@ in the @Vertex@
--     @Execution@ @Model@ @OpEntryPoint@ /must/ be 64-bit
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-format-08937# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled and the scalar width associated with a
--     @Location@ decorated @Input@ variable in the @Vertex@ @Execution@
--     @Model@ @OpEntryPoint@ is 64-bit, then the corresponding
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--     /must/ have a 64-bit component
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-09203# If there is a shader
--     object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled and
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--     has a 64-bit component, then all @Input@ variables at the
--     corresponding @Location@ in the @Vertex@ @Execution@ @Model@
--     @OpEntryPoint@ /must/ not use components that are not present in the
--     format
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04875# If there is a shader
--     object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     in the current command buffer set @primitiveTopology@ to
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04879# If there is a shader
--     object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-stage-06481# The bound graphics
--     pipeline /must/ not have been created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-08885# There /must/ be no
--     shader object bound to either of the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stages
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-transformFeedback-02287#
--     'PhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-transformFeedbackDraw-02288# The
--     implementation /must/ support
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackDraw@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-vertexStride-02289#
--     @vertexStride@ /must/ be greater than 0 and less than or equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBufferDataStride@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-counterBuffer-04567# If
--     @counterBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-counterBuffer-02290#
--     @counterBuffer@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-counterBufferOffset-04568#
--     @counterBufferOffset@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-commandBuffer-02646#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-counterBuffer-parameter#
--     @counterBuffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-renderpass# This command /must/
--     only be called inside of a render pass instance
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-commonparent# Both of
--     @commandBuffer@, and @counterBuffer@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdDrawIndirectByteCountEXT :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command is
                               -- recorded.
                               CommandBuffer
                            -> -- | @instanceCount@ is the number of instances to draw.
                               ("instanceCount" ::: Word32)
                            -> -- | @firstInstance@ is the instance ID of the first instance to draw.
                               ("firstInstance" ::: Word32)
                            -> -- | @counterBuffer@ is the buffer handle from where the byte count is read.
                               ("counterBuffer" ::: Buffer)
                            -> -- | @counterBufferOffset@ is the offset into the buffer used to read the
                               -- byte count, which is used to calculate the vertex count for this draw
                               -- call.
                               ("counterBufferOffset" ::: DeviceSize)
                            -> -- | @counterOffset@ is subtracted from the byte count read from the
                               -- @counterBuffer@ at the @counterBufferOffset@
                               ("counterOffset" ::: Word32)
                            -> -- | @vertexStride@ is the stride in bytes between each element of the vertex
                               -- data that is used to calculate the vertex count from the counter value.
                               -- This value is typically the same value that was used in the graphics
                               -- pipeline state when the transform feedback was captured as the
                               -- @XfbStride@.
                               ("vertexStride" ::: Word32)
                            -> io ()
cmdDrawIndirectByteCountEXT commandBuffer
                              instanceCount
                              firstInstance
                              counterBuffer
                              counterBufferOffset
                              counterOffset
                              vertexStride = liftIO $ do
  let vkCmdDrawIndirectByteCountEXTPtr = pVkCmdDrawIndirectByteCountEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdDrawIndirectByteCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndirectByteCountEXT is null" Nothing Nothing
  let vkCmdDrawIndirectByteCountEXT' = mkVkCmdDrawIndirectByteCountEXT vkCmdDrawIndirectByteCountEXTPtr
  traceAroundEvent "vkCmdDrawIndirectByteCountEXT" (vkCmdDrawIndirectByteCountEXT'
                                                      (commandBufferHandle (commandBuffer))
                                                      (instanceCount)
                                                      (firstInstance)
                                                      (counterBuffer)
                                                      (counterBufferOffset)
                                                      (counterOffset)
                                                      (vertexStride))
  pure $ ()


-- | VkPhysicalDeviceTransformFeedbackFeaturesEXT - Structure describing
-- transform feedback features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceTransformFeedbackFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceTransformFeedbackFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTransformFeedbackFeaturesEXT = PhysicalDeviceTransformFeedbackFeaturesEXT
  { -- | #features-transformFeedback# @transformFeedback@ indicates whether the
    -- implementation supports transform feedback and shader modules /can/
    -- declare the @TransformFeedback@ capability.
    transformFeedback :: Bool
  , -- | #features-geometryStreams# @geometryStreams@ indicates whether the
    -- implementation supports the @GeometryStreams@ SPIR-V capability.
    geometryStreams :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTransformFeedbackFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceTransformFeedbackFeaturesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTransformFeedbackFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (transformFeedback))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (geometryStreams))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTransformFeedbackFeaturesEXT where
  peekCStruct p = do
    transformFeedback <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    geometryStreams <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceTransformFeedbackFeaturesEXT
             (bool32ToBool transformFeedback) (bool32ToBool geometryStreams)

instance Storable PhysicalDeviceTransformFeedbackFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTransformFeedbackFeaturesEXT where
  zero = PhysicalDeviceTransformFeedbackFeaturesEXT
           zero
           zero


-- | VkPhysicalDeviceTransformFeedbackPropertiesEXT - Structure describing
-- transform feedback properties that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceTransformFeedbackPropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTransformFeedbackPropertiesEXT = PhysicalDeviceTransformFeedbackPropertiesEXT
  { -- | #limits-maxTransformFeedbackStreams# @maxTransformFeedbackStreams@ is
    -- the maximum number of vertex streams that can be output from geometry
    -- shaders declared with the @GeometryStreams@ capability. If the
    -- implementation does not support
    -- 'PhysicalDeviceTransformFeedbackFeaturesEXT'::@geometryStreams@ then
    -- @maxTransformFeedbackStreams@ /must/ be set to @1@.
    maxTransformFeedbackStreams :: Word32
  , -- | #limits-maxTransformFeedbackBuffers# @maxTransformFeedbackBuffers@ is
    -- the maximum number of transform feedback buffers that can be bound for
    -- capturing shader outputs from the last
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>.
    maxTransformFeedbackBuffers :: Word32
  , -- | #limits-maxTransformFeedbackBufferSize# @maxTransformFeedbackBufferSize@
    -- is the maximum size that can be specified when binding a buffer for
    -- transform feedback in 'cmdBindTransformFeedbackBuffersEXT'.
    maxTransformFeedbackBufferSize :: DeviceSize
  , -- | #limits-maxTransformFeedbackStreamDataSize#
    -- @maxTransformFeedbackStreamDataSize@ is the maximum amount of data in
    -- bytes for each vertex that captured to one or more transform feedback
    -- buffers associated with a specific vertex stream.
    maxTransformFeedbackStreamDataSize :: Word32
  , -- | #limits-maxTransformFeedbackBufferDataSize#
    -- @maxTransformFeedbackBufferDataSize@ is the maximum amount of data in
    -- bytes for each vertex that can be captured to a specific transform
    -- feedback buffer.
    maxTransformFeedbackBufferDataSize :: Word32
  , -- | #limits-maxTransformFeedbackBufferDataStride#
    -- @maxTransformFeedbackBufferDataStride@ is the maximum stride between
    -- each capture of vertex data to the buffer.
    maxTransformFeedbackBufferDataStride :: Word32
  , -- | #limits-transformFeedbackQueries# @transformFeedbackQueries@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports the
    -- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
    -- query type. @transformFeedbackQueries@ is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' if queries of this type /cannot/
    -- be created.
    transformFeedbackQueries :: Bool
  , -- | #limits-transformFeedbackStreamsLinesTriangles#
    -- @transformFeedbackStreamsLinesTriangles@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports the
    -- geometry shader @OpExecutionMode@ of @OutputLineStrip@ and
    -- @OutputTriangleStrip@ in addition to @OutputPoints@ when more than one
    -- vertex stream is output. If @transformFeedbackStreamsLinesTriangles@ is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' the implementation only supports
    -- an @OpExecutionMode@ of @OutputPoints@ when more than one vertex stream
    -- is output from the geometry shader.
    transformFeedbackStreamsLinesTriangles :: Bool
  , -- | #limits-transformFeedbackRasterizationStreamSelect#
    -- @transformFeedbackRasterizationStreamSelect@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports the
    -- @GeometryStreams@ SPIR-V capability and the application can use
    -- 'PipelineRasterizationStateStreamCreateInfoEXT' to modify which vertex
    -- stream output is used for rasterization. Otherwise vertex stream @0@
    -- /must/ always be used for rasterization.
    transformFeedbackRasterizationStreamSelect :: Bool
  , -- | #limits-transformFeedbackDraw# @transformFeedbackDraw@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports the
    -- 'cmdDrawIndirectByteCountEXT' function otherwise the function /must/ not
    -- be called.
    transformFeedbackDraw :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTransformFeedbackPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceTransformFeedbackPropertiesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackPropertiesEXT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTransformFeedbackPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxTransformFeedbackStreams)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxTransformFeedbackBuffers)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (maxTransformFeedbackBufferSize)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxTransformFeedbackStreamDataSize)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxTransformFeedbackBufferDataSize)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxTransformFeedbackBufferDataStride)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (transformFeedbackQueries))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (transformFeedbackStreamsLinesTriangles))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (transformFeedbackRasterizationStreamSelect))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (transformFeedbackDraw))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTransformFeedbackPropertiesEXT where
  peekCStruct p = do
    maxTransformFeedbackStreams <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxTransformFeedbackBuffers <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxTransformFeedbackBufferSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    maxTransformFeedbackStreamDataSize <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxTransformFeedbackBufferDataSize <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxTransformFeedbackBufferDataStride <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    transformFeedbackQueries <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    transformFeedbackStreamsLinesTriangles <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    transformFeedbackRasterizationStreamSelect <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    transformFeedbackDraw <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    pure $ PhysicalDeviceTransformFeedbackPropertiesEXT
             maxTransformFeedbackStreams
             maxTransformFeedbackBuffers
             maxTransformFeedbackBufferSize
             maxTransformFeedbackStreamDataSize
             maxTransformFeedbackBufferDataSize
             maxTransformFeedbackBufferDataStride
             (bool32ToBool transformFeedbackQueries)
             (bool32ToBool transformFeedbackStreamsLinesTriangles)
             (bool32ToBool transformFeedbackRasterizationStreamSelect)
             (bool32ToBool transformFeedbackDraw)

instance Storable PhysicalDeviceTransformFeedbackPropertiesEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTransformFeedbackPropertiesEXT where
  zero = PhysicalDeviceTransformFeedbackPropertiesEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPipelineRasterizationStateStreamCreateInfoEXT - Structure defining the
-- geometry stream used for rasterization
--
-- = Description
--
-- If this structure is not present, @rasterizationStream@ is assumed to be
-- zero.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'PipelineRasterizationStateStreamCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationStateStreamCreateInfoEXT = PipelineRasterizationStateStreamCreateInfoEXT
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkPipelineRasterizationStateStreamCreateInfoEXT-flags-zerobitmask#
    -- @flags@ /must/ be @0@
    flags :: PipelineRasterizationStateStreamCreateFlagsEXT
  , -- | @rasterizationStream@ is the vertex stream selected for rasterization.
    --
    -- #VUID-VkPipelineRasterizationStateStreamCreateInfoEXT-rasterizationStream-02325#
    -- @rasterizationStream@ /must/ be less than
    -- 'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
    --
    -- #VUID-VkPipelineRasterizationStateStreamCreateInfoEXT-rasterizationStream-02326#
    -- @rasterizationStream@ /must/ be zero if
    -- 'PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackRasterizationStreamSelect@
    -- is 'Vulkan.Core10.FundamentalTypes.FALSE'
    rasterizationStream :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationStateStreamCreateInfoEXT)
#endif
deriving instance Show PipelineRasterizationStateStreamCreateInfoEXT

instance ToCStruct PipelineRasterizationStateStreamCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationStateStreamCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRasterizationStateStreamCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (rasterizationStream)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineRasterizationStateStreamCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineRasterizationStateStreamCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineRasterizationStateStreamCreateFlagsEXT))
    rasterizationStream <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PipelineRasterizationStateStreamCreateInfoEXT
             flags rasterizationStream

instance Storable PipelineRasterizationStateStreamCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationStateStreamCreateInfoEXT where
  zero = PipelineRasterizationStateStreamCreateInfoEXT
           zero
           zero


-- | VkPipelineRasterizationStateStreamCreateFlagsEXT - Reserved for future
-- use
--
-- = Description
--
-- 'PipelineRasterizationStateStreamCreateFlagsEXT' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>,
-- 'PipelineRasterizationStateStreamCreateInfoEXT'
newtype PipelineRasterizationStateStreamCreateFlagsEXT = PipelineRasterizationStateStreamCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNamePipelineRasterizationStateStreamCreateFlagsEXT :: String
conNamePipelineRasterizationStateStreamCreateFlagsEXT = "PipelineRasterizationStateStreamCreateFlagsEXT"

enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT :: String
enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT = ""

showTablePipelineRasterizationStateStreamCreateFlagsEXT :: [(PipelineRasterizationStateStreamCreateFlagsEXT, String)]
showTablePipelineRasterizationStateStreamCreateFlagsEXT = []

instance Show PipelineRasterizationStateStreamCreateFlagsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT
      showTablePipelineRasterizationStateStreamCreateFlagsEXT
      conNamePipelineRasterizationStateStreamCreateFlagsEXT
      (\(PipelineRasterizationStateStreamCreateFlagsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineRasterizationStateStreamCreateFlagsEXT where
  readPrec =
    enumReadPrec
      enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT
      showTablePipelineRasterizationStateStreamCreateFlagsEXT
      conNamePipelineRasterizationStateStreamCreateFlagsEXT
      PipelineRasterizationStateStreamCreateFlagsEXT

type EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION"
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1


type EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = "VK_EXT_transform_feedback"

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME"
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = "VK_EXT_transform_feedback"

