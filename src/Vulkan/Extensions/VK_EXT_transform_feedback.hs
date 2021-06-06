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
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_transform_feedback:%20&body=@pdaniell-nv%20 >
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
--     -   Jason Ekstrand, Intel
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
-- stream output from the last vertex processing stage. The vertex stream
-- output queried is zero by default, but can be specified with the new
-- 'cmdBeginQueryIndexedEXT' and 'cmdEndQueryIndexedEXT' commands.
--
-- == Version History
--
-- -   Revision 1, 2018-10-09 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceTransformFeedbackFeaturesEXT',
-- 'PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'PipelineRasterizationStateStreamCreateFlagsEXT',
-- 'PipelineRasterizationStateStreamCreateInfoEXT',
-- 'cmdBeginQueryIndexedEXT', 'cmdBeginTransformFeedbackEXT',
-- 'cmdBindTransformFeedbackBuffersEXT', 'cmdDrawIndirectByteCountEXT',
-- 'cmdEndQueryIndexedEXT', 'cmdEndTransformFeedbackEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback Vulkan Specification>
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

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
cmdBindTransformFeedbackBuffersEXT commandBuffer firstBinding buffers offsets sizes = liftIO . evalContT $ do
  let vkCmdBindTransformFeedbackBuffersEXTPtr = pVkCmdBindTransformFeedbackBuffersEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindTransformFeedbackBuffersEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindTransformFeedbackBuffersEXT is null" Nothing Nothing
  let vkCmdBindTransformFeedbackBuffersEXT' = mkVkCmdBindTransformFeedbackBuffersEXT vkCmdBindTransformFeedbackBuffersEXTPtr
  let pBuffersLength = Data.Vector.length $ (buffers)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  let pSizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral pSizesLength == pBuffersLength || pSizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pSizes and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (buffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytesAligned @DeviceSize ((Data.Vector.length (offsets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  pSizes <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pPSizes <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (sizes))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pPSizes
  lift $ traceAroundEvent "vkCmdBindTransformFeedbackBuffersEXT" (vkCmdBindTransformFeedbackBuffersEXT' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets) pSizes)
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
-- -   #VUID-vkCmdBeginTransformFeedbackEXT-None-04128# The last vertex
--     processing stage of the bound graphics pipeline /must/ have been
--     declared with the @Xfb@ execution mode
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
cmdBeginTransformFeedbackEXT commandBuffer firstCounterBuffer counterBuffers counterBufferOffsets = liftIO . evalContT $ do
  let vkCmdBeginTransformFeedbackEXTPtr = pVkCmdBeginTransformFeedbackEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginTransformFeedbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginTransformFeedbackEXT is null" Nothing Nothing
  let vkCmdBeginTransformFeedbackEXT' = mkVkCmdBeginTransformFeedbackEXT vkCmdBeginTransformFeedbackEXTPtr
  let pCounterBuffersLength = Data.Vector.length $ (counterBuffers)
  let pCounterBufferOffsetsLength = Data.Vector.length $ (counterBufferOffsets)
  lift $ unless (fromIntegral pCounterBufferOffsetsLength == pCounterBuffersLength || pCounterBufferOffsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pCounterBufferOffsets and pCounterBuffers must have the same length" Nothing Nothing
  pPCounterBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (counterBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (counterBuffers)
  pCounterBufferOffsets <- if Data.Vector.null (counterBufferOffsets)
    then pure nullPtr
    else do
      pPCounterBufferOffsets <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (counterBufferOffsets))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBufferOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((counterBufferOffsets))
      pure $ pPCounterBufferOffsets
  lift $ traceAroundEvent "vkCmdBeginTransformFeedbackEXT" (vkCmdBeginTransformFeedbackEXT' (commandBufferHandle (commandBuffer)) (firstCounterBuffer) ((fromIntegral pCounterBuffersLength :: Word32)) (pPCounterBuffers) pCounterBufferOffsets)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginTransformFeedbackEXT' and 'cmdEndTransformFeedbackEXT'
--
-- Note that 'cmdEndTransformFeedbackEXT' is *not* called if an exception
-- is thrown by the inner action.
cmdUseTransformFeedbackEXT :: forall io r . MonadIO io => CommandBuffer -> Word32 -> Vector Buffer -> Vector DeviceSize -> io r -> io r
cmdUseTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets a =
  (cmdBeginTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets) *> a <* (cmdEndTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets)


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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
cmdEndTransformFeedbackEXT commandBuffer firstCounterBuffer counterBuffers counterBufferOffsets = liftIO . evalContT $ do
  let vkCmdEndTransformFeedbackEXTPtr = pVkCmdEndTransformFeedbackEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdEndTransformFeedbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndTransformFeedbackEXT is null" Nothing Nothing
  let vkCmdEndTransformFeedbackEXT' = mkVkCmdEndTransformFeedbackEXT vkCmdEndTransformFeedbackEXTPtr
  let pCounterBuffersLength = Data.Vector.length $ (counterBuffers)
  let pCounterBufferOffsetsLength = Data.Vector.length $ (counterBufferOffsets)
  lift $ unless (fromIntegral pCounterBufferOffsetsLength == pCounterBuffersLength || pCounterBufferOffsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pCounterBufferOffsets and pCounterBuffers must have the same length" Nothing Nothing
  pPCounterBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (counterBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (counterBuffers)
  pCounterBufferOffsets <- if Data.Vector.null (counterBufferOffsets)
    then pure nullPtr
    else do
      pPCounterBufferOffsets <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (counterBufferOffsets))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBufferOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((counterBufferOffsets))
      pure $ pPCounterBufferOffsets
  lift $ traceAroundEvent "vkCmdEndTransformFeedbackEXT" (vkCmdEndTransformFeedbackEXT' (commandBufferHandle (commandBuffer)) (firstCounterBuffer) ((fromIntegral pCounterBuffersLength :: Word32)) (pPCounterBuffers) pCounterBufferOffsets)
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
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-None-00807# All queries used by the
--     command /must/ be unavailable
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
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-04729# The @queryType@
--     used to create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-00800# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-occlusionQueryPrecise precise occlusion queries>
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
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryPool-04753# If the @queryPool@
--     was created with the same @queryType@ as that of another
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
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
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-02340# If the @queryType@
--     used to create @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ /must/ be zero
--
-- -   #VUID-vkCmdBeginQueryIndexedEXT-queryType-02341# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     then
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackQueries@
--     /must/ be supported
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
--     allocated from /must/ support graphics, or compute operations
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
-- 'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlags',
-- 'Vulkan.Core10.Handles.QueryPool'
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
                           -- the index represents the vertex stream.
                           ("index" ::: Word32)
                        -> io ()
cmdBeginQueryIndexedEXT commandBuffer queryPool query flags index = liftIO $ do
  let vkCmdBeginQueryIndexedEXTPtr = pVkCmdBeginQueryIndexedEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBeginQueryIndexedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginQueryIndexedEXT is null" Nothing Nothing
  let vkCmdBeginQueryIndexedEXT' = mkVkCmdBeginQueryIndexedEXT vkCmdBeginQueryIndexedEXTPtr
  traceAroundEvent "vkCmdBeginQueryIndexedEXT" (vkCmdBeginQueryIndexedEXT' (commandBufferHandle (commandBuffer)) (queryPool) (query) (flags) (index))
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginQueryIndexedEXT' and 'cmdEndQueryIndexedEXT'
--
-- Note that 'cmdEndQueryIndexedEXT' is *not* called if an exception is
-- thrown by the inner action.
cmdUseQueryIndexedEXT :: forall io r . MonadIO io => CommandBuffer -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> io r -> io r
cmdUseQueryIndexedEXT commandBuffer queryPool query flags index a =
  (cmdBeginQueryIndexedEXT commandBuffer queryPool query flags index) *> a <* (cmdEndQueryIndexedEXT commandBuffer queryPool query index)


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
-- The 'cmdEndQueryIndexedEXT' command operates the same as the
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery' command, except that
-- it also accepts a query type specific @index@ parameter.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-None-02342# All queries used by the
--     command /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
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
-- -   #VUID-vkCmdEndQueryIndexedEXT-queryType-02346# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ parameter /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-queryType-02347# If the @queryType@
--     used to create @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ /must/ be zero
--
-- -   #VUID-vkCmdEndQueryIndexedEXT-queryType-02723# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     @index@ /must/ equal the @index@ used to begin the query
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
--     allocated from /must/ support graphics, or compute operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.QueryPool'
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
  let vkCmdEndQueryIndexedEXTPtr = pVkCmdEndQueryIndexedEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndQueryIndexedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndQueryIndexedEXT is null" Nothing Nothing
  let vkCmdEndQueryIndexedEXT' = mkVkCmdEndQueryIndexedEXT vkCmdEndQueryIndexedEXTPtr
  traceAroundEvent "vkCmdEndQueryIndexedEXT" (vkCmdEndQueryIndexedEXT' (commandBufferHandle (commandBuffer)) (queryPool) (query) (index))
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
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
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
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02697# For each set /n/
--     that is statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command, a descriptor
--     set /must/ have been bound to /n/ at the same pipeline bind point,
--     with a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for
--     set /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02698# For each push
--     constant that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a push constant value /must/ have been set for
--     the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02699# Descriptors in each
--     bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02700# A valid pipeline
--     /must/ be bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set or inherited for @commandBuffer@, and done so
--     after any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02859# There /must/ not
--     have been any calls to dynamic state setting commands for any state
--     not specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command, since
--     that pipeline was bound
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
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
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format.
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sparseImageInt64Atomics-04474#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-sparseImageInt64Atomics-04475#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
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
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-02686# Every input
--     attachment used by the current subpass /must/ be bound to the
--     pipeline via a descriptor set
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04584# Image subresources
--     used as attachments in the current render pass /must/ not be
--     accessed in any way other than as an attachment by this command,
--     except for cases involving read-only access to depth\/stencil
--     attachments as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
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
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-03417# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-scissorCount-03418# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-03419# If the
--     bound graphics pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-04137# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-04138# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-04139# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-viewportCount-04140# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-VkPipelineVieportCreateInfo-04141#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-VkPipelineVieportCreateInfo-04142#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-primitiveTopology-03420# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04875# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04876# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04877# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnableEXT'
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
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-None-04879# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
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
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-rasterizationSamples-04740# If
--     rasterization is not disabled in the bound graphics pipeline, and
--     neither the @@ nor the @@ extensions are enabled, then
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     /must/ be the same as the current subpass color and\/or
--     depth\/stencil attachments
--
-- -   #VUID-vkCmdDrawIndirectByteCountEXT-pStrides-04884# If the bound
--     graphics pipeline was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @pStrides@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ not be @NULL@
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
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTransformFeedbackBufferDataStride@
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
cmdDrawIndirectByteCountEXT commandBuffer instanceCount firstInstance counterBuffer counterBufferOffset counterOffset vertexStride = liftIO $ do
  let vkCmdDrawIndirectByteCountEXTPtr = pVkCmdDrawIndirectByteCountEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndirectByteCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndirectByteCountEXT is null" Nothing Nothing
  let vkCmdDrawIndirectByteCountEXT' = mkVkCmdDrawIndirectByteCountEXT vkCmdDrawIndirectByteCountEXTPtr
  traceAroundEvent "vkCmdDrawIndirectByteCountEXT" (vkCmdDrawIndirectByteCountEXT' (commandBufferHandle (commandBuffer)) (instanceCount) (firstInstance) (counterBuffer) (counterBufferOffset) (counterOffset) (vertexStride))
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
    -- capturing shader outputs from the last vertex processing stage.
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
  , -- | #limits-transformFeedbackQueries# @transformFeedbackQueries@ is true if
    -- the implementation supports the
    -- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
    -- query type. @transformFeedbackQueries@ is false if queries of this type
    -- /cannot/ be created.
    transformFeedbackQueries :: Bool
  , -- | #limits-transformFeedbackStreamsLinesTriangles#
    -- @transformFeedbackStreamsLinesTriangles@ is true if the implementation
    -- supports the geometry shader @OpExecutionMode@ of @OutputLineStrip@ and
    -- @OutputTriangleStrip@ in addition to @OutputPoints@ when more than one
    -- vertex stream is output. If @transformFeedbackStreamsLinesTriangles@ is
    -- false the implementation only supports an @OpExecutionMode@ of
    -- @OutputPoints@ when more than one vertex stream is output from the
    -- geometry shader.
    transformFeedbackStreamsLinesTriangles :: Bool
  , -- | #limits-transformFeedbackRasterizationStreamSelect#
    -- @transformFeedbackRasterizationStreamSelect@ is true if the
    -- implementation supports the @GeometryStreams@ SPIR-V capability and the
    -- application can use 'PipelineRasterizationStateStreamCreateInfoEXT' to
    -- modify which vertex stream output is used for rasterization. Otherwise
    -- vertex stream @0@ /must/ always be used for rasterization.
    transformFeedbackRasterizationStreamSelect :: Bool
  , -- | #limits-transformFeedbackDraw# @transformFeedbackDraw@ is true if the
    -- implementation supports the 'cmdDrawIndirectByteCountEXT' function
    -- otherwise the function /must/ not be called.
    transformFeedbackDraw :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTransformFeedbackPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceTransformFeedbackPropertiesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackPropertiesEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
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
             maxTransformFeedbackStreams maxTransformFeedbackBuffers maxTransformFeedbackBufferSize maxTransformFeedbackStreamDataSize maxTransformFeedbackBufferDataSize maxTransformFeedbackBufferDataStride (bool32ToBool transformFeedbackQueries) (bool32ToBool transformFeedbackStreamsLinesTriangles) (bool32ToBool transformFeedbackRasterizationStreamSelect) (bool32ToBool transformFeedbackDraw)

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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
  showsPrec = enumShowsPrec enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT
                            showTablePipelineRasterizationStateStreamCreateFlagsEXT
                            conNamePipelineRasterizationStateStreamCreateFlagsEXT
                            (\(PipelineRasterizationStateStreamCreateFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineRasterizationStateStreamCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT
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

