{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_transform_feedback
  ( withCStructPhysicalDeviceTransformFeedbackFeaturesEXT
  , fromCStructPhysicalDeviceTransformFeedbackFeaturesEXT
  , PhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , withCStructPhysicalDeviceTransformFeedbackPropertiesEXT
  , fromCStructPhysicalDeviceTransformFeedbackPropertiesEXT
  , PhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , PipelineRasterizationStateStreamCreateFlagsEXT
  , withCStructPipelineRasterizationStateStreamCreateInfoEXT
  , fromCStructPipelineRasterizationStateStreamCreateInfoEXT
  , PipelineRasterizationStateStreamCreateInfoEXT(..)
  , cmdBeginQueryIndexedEXT
  , cmdBeginTransformFeedbackEXT
  , cmdBindTransformFeedbackBuffersEXT
  , cmdDrawIndirectByteCountEXT
  , cmdEndQueryIndexedEXT
  , cmdEndTransformFeedbackEXT
  , pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
  , pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
  , pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
  , pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
  , pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( VkPhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , VkPhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , VkPipelineRasterizationStateStreamCreateFlagsEXT(..)
  , VkPipelineRasterizationStateStreamCreateInfoEXT(..)
  , vkCmdBeginQueryIndexedEXT
  , vkCmdBeginTransformFeedbackEXT
  , vkCmdBindTransformFeedbackBuffersEXT
  , vkCmdDrawIndirectByteCountEXT
  , vkCmdEndQueryIndexedEXT
  , vkCmdEndTransformFeedbackEXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.CommandBuffer
  ( QueryControlFlags
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPool
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
  , pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  , pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
  , pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
  )



-- | VkPhysicalDeviceTransformFeedbackFeaturesEXT - Structure describing
-- transform feedback features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in VkPhysicalDeviceTransformFeedbackFeaturesEXT.txt
-- -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceTransformFeedbackFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceTransformFeedbackFeaturesEXT = PhysicalDeviceTransformFeedbackFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "transformFeedback"
  transformFeedback :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "geometryStreams"
  geometryStreams :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceTransformFeedbackFeaturesEXT' and
-- marshal a 'PhysicalDeviceTransformFeedbackFeaturesEXT' into it. The 'VkPhysicalDeviceTransformFeedbackFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceTransformFeedbackFeaturesEXT :: PhysicalDeviceTransformFeedbackFeaturesEXT -> (VkPhysicalDeviceTransformFeedbackFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceTransformFeedbackFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceTransformFeedbackFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceTransformFeedbackFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT pPNext (boolToBool32 (transformFeedback (marshalled :: PhysicalDeviceTransformFeedbackFeaturesEXT))) (boolToBool32 (geometryStreams (marshalled :: PhysicalDeviceTransformFeedbackFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceTransformFeedbackFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceTransformFeedbackFeaturesEXT'.
fromCStructPhysicalDeviceTransformFeedbackFeaturesEXT :: VkPhysicalDeviceTransformFeedbackFeaturesEXT -> IO PhysicalDeviceTransformFeedbackFeaturesEXT
fromCStructPhysicalDeviceTransformFeedbackFeaturesEXT c = PhysicalDeviceTransformFeedbackFeaturesEXT <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceTransformFeedbackFeaturesEXT)))
                                                                                                     <*> pure (bool32ToBool (vkTransformFeedback (c :: VkPhysicalDeviceTransformFeedbackFeaturesEXT)))
                                                                                                     <*> pure (bool32ToBool (vkGeometryStreams (c :: VkPhysicalDeviceTransformFeedbackFeaturesEXT)))

instance Zero PhysicalDeviceTransformFeedbackFeaturesEXT where
  zero = PhysicalDeviceTransformFeedbackFeaturesEXT Nothing
                                                    False
                                                    False



-- | VkPhysicalDeviceTransformFeedbackPropertiesEXT - Structure describing
-- transform feedback properties that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits and properties.
--
-- Unresolved directive in
-- VkPhysicalDeviceTransformFeedbackPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceTransformFeedbackPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceTransformFeedbackPropertiesEXT = PhysicalDeviceTransformFeedbackPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackStreams"
  maxTransformFeedbackStreams :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBuffers"
  maxTransformFeedbackBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferSize"
  maxTransformFeedbackBufferSize :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackStreamDataSize"
  maxTransformFeedbackStreamDataSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferDataSize"
  maxTransformFeedbackBufferDataSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferDataStride"
  maxTransformFeedbackBufferDataStride :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackQueries"
  transformFeedbackQueries :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackStreamsLinesTriangles"
  transformFeedbackStreamsLinesTriangles :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackRasterizationStreamSelect"
  transformFeedbackRasterizationStreamSelect :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackDraw"
  transformFeedbackDraw :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceTransformFeedbackPropertiesEXT' and
-- marshal a 'PhysicalDeviceTransformFeedbackPropertiesEXT' into it. The 'VkPhysicalDeviceTransformFeedbackPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceTransformFeedbackPropertiesEXT :: PhysicalDeviceTransformFeedbackPropertiesEXT -> (VkPhysicalDeviceTransformFeedbackPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceTransformFeedbackPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceTransformFeedbackPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT pPNext (maxTransformFeedbackStreams (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (maxTransformFeedbackBuffers (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (maxTransformFeedbackBufferSize (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (maxTransformFeedbackStreamDataSize (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (maxTransformFeedbackBufferDataSize (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (maxTransformFeedbackBufferDataStride (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (boolToBool32 (transformFeedbackQueries (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT))) (boolToBool32 (transformFeedbackStreamsLinesTriangles (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT))) (boolToBool32 (transformFeedbackRasterizationStreamSelect (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT))) (boolToBool32 (transformFeedbackDraw (marshalled :: PhysicalDeviceTransformFeedbackPropertiesEXT)))))

-- | A function to read a 'VkPhysicalDeviceTransformFeedbackPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceTransformFeedbackPropertiesEXT'.
fromCStructPhysicalDeviceTransformFeedbackPropertiesEXT :: VkPhysicalDeviceTransformFeedbackPropertiesEXT -> IO PhysicalDeviceTransformFeedbackPropertiesEXT
fromCStructPhysicalDeviceTransformFeedbackPropertiesEXT c = PhysicalDeviceTransformFeedbackPropertiesEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT)))
                                                                                                         <*> pure (vkMaxTransformFeedbackStreams (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                                                                                                         <*> pure (vkMaxTransformFeedbackBuffers (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                                                                                                         <*> pure (vkMaxTransformFeedbackBufferSize (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                                                                                                         <*> pure (vkMaxTransformFeedbackStreamDataSize (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                                                                                                         <*> pure (vkMaxTransformFeedbackBufferDataSize (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                                                                                                         <*> pure (vkMaxTransformFeedbackBufferDataStride (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                                                                                                         <*> pure (bool32ToBool (vkTransformFeedbackQueries (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkTransformFeedbackStreamsLinesTriangles (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkTransformFeedbackRasterizationStreamSelect (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkTransformFeedbackDraw (c :: VkPhysicalDeviceTransformFeedbackPropertiesEXT)))

instance Zero PhysicalDeviceTransformFeedbackPropertiesEXT where
  zero = PhysicalDeviceTransformFeedbackPropertiesEXT Nothing
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      False
                                                      False
                                                      False
                                                      False


-- | VkPipelineRasterizationStateStreamCreateFlagsEXT - Reserved for future
-- use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPipelineRasterizationStateStreamCreateFlagsEXT'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- No cross-references are available
type PipelineRasterizationStateStreamCreateFlagsEXT = VkPipelineRasterizationStateStreamCreateFlagsEXT


-- | VkPipelineRasterizationStateStreamCreateInfoEXT - Structure defining the
-- geometry stream used for rasterization
--
-- = Description
--
-- If this structure is not present, @rasterizationStream@ is assumed to be
-- zero.
--
-- == Valid Usage
--
-- Unresolved directive in
-- VkPipelineRasterizationStateStreamCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkPipelineRasterizationStateStreamCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineRasterizationStateStreamCreateInfoEXT = PipelineRasterizationStateStreamCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "flags"
  flags :: PipelineRasterizationStateStreamCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "rasterizationStream"
  rasterizationStream :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineRasterizationStateStreamCreateInfoEXT' and
-- marshal a 'PipelineRasterizationStateStreamCreateInfoEXT' into it. The 'VkPipelineRasterizationStateStreamCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineRasterizationStateStreamCreateInfoEXT :: PipelineRasterizationStateStreamCreateInfoEXT -> (VkPipelineRasterizationStateStreamCreateInfoEXT -> IO a) -> IO a
withCStructPipelineRasterizationStateStreamCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineRasterizationStateStreamCreateInfoEXT)) (\pPNext -> cont (VkPipelineRasterizationStateStreamCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT pPNext (flags (marshalled :: PipelineRasterizationStateStreamCreateInfoEXT)) (rasterizationStream (marshalled :: PipelineRasterizationStateStreamCreateInfoEXT))))

-- | A function to read a 'VkPipelineRasterizationStateStreamCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'PipelineRasterizationStateStreamCreateInfoEXT'.
fromCStructPipelineRasterizationStateStreamCreateInfoEXT :: VkPipelineRasterizationStateStreamCreateInfoEXT -> IO PipelineRasterizationStateStreamCreateInfoEXT
fromCStructPipelineRasterizationStateStreamCreateInfoEXT c = PipelineRasterizationStateStreamCreateInfoEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationStateStreamCreateInfoEXT)))
                                                                                                           <*> pure (vkFlags (c :: VkPipelineRasterizationStateStreamCreateInfoEXT))
                                                                                                           <*> pure (vkRasterizationStream (c :: VkPipelineRasterizationStateStreamCreateInfoEXT))

instance Zero PipelineRasterizationStateStreamCreateInfoEXT where
  zero = PipelineRasterizationStateStreamCreateInfoEXT Nothing
                                                       zero
                                                       zero



-- | vkCmdBeginQueryIndexedEXT - Begin an indexed query
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool that will manage the results of the
--     query.
--
-- -   @query@ is the query index within the query pool that will contain
--     the results.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits'
--     specifying constraints on the types of queries that /can/ be
--     performed.
--
-- -   @index@ is the query type specific index. When the query type is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the index represents the vertex stream.
--
-- = Description
--
-- The
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBeginQueryIndexedEXT'
-- command operates the same as the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery'
-- command, except that it also accepts a query type specific @index@
-- parameter.
--
-- == Valid Usage
--
-- -   @queryPool@ /must/ have been created with a @queryType@ that differs
--     from that of any queries that are
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--     within @commandBuffer@
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-occlusionQueryPrecise precise occlusion queries>
--     feature is not enabled, or the @queryType@ used to create
--     @queryPool@ was not
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_QUERY_CONTROL_PRECISE_BIT'
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION', the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
--     and any of the @pipelineStatistics@ indicate graphics operations,
--     the 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
--     and any of the @pipelineStatistics@ indicate compute operations, the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   If 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery'
--     is called within a render pass instance, the sum of @query@ and the
--     number of bits set in the current subpass’s view mask /must/ be less
--     than or equal to the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ parameter /must/ be less than
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   If the @queryType@ used to create @queryPool@ was not
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ /must/ be zero
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     then
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackQueries@
--     /must/ be supported
--
-- Unresolved directive in vkCmdBeginQueryIndexedEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdBeginQueryIndexedEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdBeginQueryIndexedEXT :: CommandBuffer ->  QueryPool ->  Word32 ->  QueryControlFlags ->  Word32 ->  IO ()
cmdBeginQueryIndexedEXT = \(CommandBuffer commandBuffer' commandTable) -> \queryPool' -> \query' -> \flags' -> \index' -> vkCmdBeginQueryIndexedEXT commandTable commandBuffer' queryPool' query' flags' index' *> (pure ())


-- | vkCmdBeginTransformFeedbackEXT - Make transform feedback active in the
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstCounterBuffer@ is the index of the first transform feedback
--     buffer corresponding to @pCounterBuffers@[0] and
--     @pCounterBufferOffsets@[0].
--
-- -   @counterBufferCount@ is the size of the @pCounterBuffers@ and
--     @pCounterBufferOffsets@ arrays.
--
-- -   @pCounterBuffers@ is an optional array of buffer handles to the
--     counter buffers which contain a 4 byte integer value representing
--     the byte offset from the start of the corresponding transform
--     feedback buffer from where to start capturing vertex data. If the
--     byte offset stored to the counter buffer location was done using
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdEndTransformFeedbackEXT'
--     it can be used to resume transform feedback from the previous
--     location. If @pCounterBuffers@ is @NULL@, then transform feedback
--     will start capturing vertex data to byte offset zero in all bound
--     transform feedback buffers. For each element of @pCounterBuffers@
--     that is 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     transform feedback will start capturing vertex data to byte zero in
--     the corresponding bound transform feedback buffer.
--
-- -   @pCounterBufferOffsets@ is an optional array of offsets within each
--     of the @pCounterBuffers@ where the counter values were previously
--     written. The location in each counter buffer at these offsets /must/
--     be large enough to contain 4 bytes of data. This data is the number
--     of bytes captured by the previous transform feedback to this buffer.
--     If @pCounterBufferOffsets@ is @NULL@, then it is assumed the offsets
--     are zero.
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
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   Transform feedback /must/ not be active
--
-- -   @firstCounterBuffer@ /must/ be less than
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstCounterBuffer@ and @counterBufferCount@ /must/ be
--     less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBuffers@ is not
--     @NULL@, @pCounterBuffers@ /must/ be a valid pointer to an array of
--     @counterBufferCount@
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handles that
--     are either valid or
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   For each buffer handle in the array, if it is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/
--     reference a buffer large enough to hold 4 bytes at the corresponding
--     offset from the @pCounterBufferOffsets@ array
--
-- -   If @pCounterBuffer@ is @NULL@, then @pCounterBufferOffsets@ /must/
--     also be @NULL@
--
-- -   For each buffer handle in the @pCounterBuffers@ array that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ have
--     been created with a @usage@ value containing
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- -   Transform feedback /must/ not be made active in a render pass
--     instance with multiview enabled
--
-- Unresolved directive in vkCmdBeginTransformFeedbackEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdBeginTransformFeedbackEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdBeginTransformFeedbackEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Maybe (Vector DeviceSize) ->  IO ()
cmdBeginTransformFeedbackEXT = \(CommandBuffer commandBuffer' commandTable) -> \firstCounterBuffer' -> \counterBuffers' -> \counterBufferOffsets' -> maybeWith (withVec (&)) counterBufferOffsets' (\pCounterBufferOffsets' -> withVec (&) counterBuffers' (\pCounterBuffers' -> vkCmdBeginTransformFeedbackEXT commandTable commandBuffer' firstCounterBuffer' (fromIntegral $ Data.Vector.length counterBuffers' `min` maybe maxBound Data.Vector.length counterBufferOffsets') pCounterBuffers' pCounterBufferOffsets' *> (pure ())))


-- | vkCmdBindTransformFeedbackBuffersEXT - Bind transform feedback buffers
-- to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstBinding@ is the index of the first transform feedback binding
--     whose state is updated by the command.
--
-- -   @bindingCount@ is the number of transform feedback bindings whose
--     state is updated by the command.
--
-- -   @pBuffers@ is a pointer to an array of buffer handles.
--
-- -   @pOffsets@ is a pointer to an array of buffer offsets.
--
-- -   @pSizes@ is an optional array of buffer sizes, which specifies the
--     maximum number of bytes to capture to the corresponding transform
--     feedback buffer. If @pSizes@ is @NULL@, or the value of the @pSizes@
--     array element is 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE',
--     then the maximum bytes captured will be the size of the
--     corresponding buffer minus the buffer offset.
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
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   @firstBinding@ /must/ be less than
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstBinding@ and @bindingCount@ /must/ be less than or
--     equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   All elements of @pOffsets@ /must/ be less than the size of the
--     corresponding element in @pBuffers@
--
-- -   All elements of @pOffsets@ /must/ be a multiple of 4
--
-- -   All elements of @pBuffers@ /must/ have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT'
--     flag
--
-- -   If the optional @pSize@ array is specified, each element of @pSizes@
--     /must/ either be 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE',
--     or be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBufferSize@
--
-- -   All elements of @pSizes@ /must/ be less than or equal to the size of
--     the corresponding buffer in @pBuffers@
--
-- -   All elements of @pOffsets@ plus @pSizes@, where the @pSizes@,
--     element is not 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE',
--     /must/ be less than or equal to the size of the corresponding
--     element in @pBuffers@
--
-- -   Each element of @pBuffers@ that is non-sparse /must/ be bound
--     completely and contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   Transform feedback /must/ not be active when the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBindTransformFeedbackBuffersEXT'
--     command is recorded
--
-- Unresolved directive in vkCmdBindTransformFeedbackBuffersEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdBindTransformFeedbackBuffersEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdBindTransformFeedbackBuffersEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Vector DeviceSize ->  Maybe (Vector DeviceSize) ->  IO ()
cmdBindTransformFeedbackBuffersEXT = \(CommandBuffer commandBuffer' commandTable) -> \firstBinding' -> \buffers' -> \offsets' -> \sizes' -> maybeWith (withVec (&)) sizes' (\pSizes' -> withVec (&) offsets' (\pOffsets' -> withVec (&) buffers' (\pBuffers' -> vkCmdBindTransformFeedbackBuffersEXT commandTable commandBuffer' firstBinding' (fromIntegral $ Data.Vector.length buffers' `min` Data.Vector.length offsets' `min` maybe maxBound Data.Vector.length sizes') pBuffers' pOffsets' pSizes' *> (pure ()))))


-- | vkCmdDrawIndirectByteCountEXT - Draw primitives where the vertex count
-- is derived from the counter byte value in the counter buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @instanceCount@ is the number of instances to draw.
--
-- -   @firstInstance@ is the instance ID of the first instance to draw.
--
-- -   @counterBuffer@ is the buffer handle from where the byte count is
--     read.
--
-- -   @counterBufferOffset@ is the offset into the buffer used to read the
--     byte count, which is used to calculate the vertex count for this
--     draw call.
--
-- -   @counterOffset@ is subtracted from the byte count read from the
--     @counterBuffer@ at the @counterBufferOffset@
--
-- -   @vertexStride@ is the stride in bytes between each element of the
--     vertex data that is used to calculate the vertex count from the
--     counter value. This value is typically the same value that was used
--     in the graphics pipeline state when the transform feedback was
--     captured as the @XfbStride@.
--
-- = Description
--
-- When the command is executed, primitives are assembled in the same way
-- as done with 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDraw'
-- except the @vertexCount@ is calculated based on the byte count read from
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
-- Unresolved directive in vkCmdDrawIndirectByteCountEXT.txt -
-- include::{chapters}\/commonvalidity\/draw_common.txt[] Unresolved
-- directive in vkCmdDrawIndirectByteCountEXT.txt -
-- include::{chapters}\/commonvalidity\/draw_vertex_binding.txt[] *
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
-- /must/ be enabled * The implementation /must/ support
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackDraw@
-- * @vertexStride@ /must/ be greater than 0 and less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxTransformFeedbackBufferDataStride@
-- * @counterBuffer@ /must/ have been created with the
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
-- bit set
--
-- Unresolved directive in vkCmdDrawIndirectByteCountEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdDrawIndirectByteCountEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDrawIndirectByteCountEXT :: CommandBuffer ->  Word32 ->  Word32 ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirectByteCountEXT = \(CommandBuffer commandBuffer' commandTable) -> \instanceCount' -> \firstInstance' -> \counterBuffer' -> \counterBufferOffset' -> \counterOffset' -> \vertexStride' -> vkCmdDrawIndirectByteCountEXT commandTable commandBuffer' instanceCount' firstInstance' counterBuffer' counterBufferOffset' counterOffset' vertexStride' *> (pure ())


-- | vkCmdEndQueryIndexedEXT - Ends a query
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool that is managing the results of the
--     query.
--
-- -   @query@ is the query index within the query pool where the result is
--     stored.
--
-- -   @index@ is the query type specific index.
--
-- = Description
--
-- The
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdEndQueryIndexedEXT'
-- command operates the same as the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery' command,
-- except that it also accepts a query type specific @index@ parameter.
--
-- == Valid Usage
--
-- -   All queries used by the command /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   If 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery' is
--     called within a render pass instance, the sum of @query@ and the
--     number of bits set in the current subpass’s view mask /must/ be less
--     than or equal to the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ parameter /must/ be less than
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   If the @queryType@ used to create @queryPool@ was not
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ /must/ be zero
--
-- Unresolved directive in vkCmdEndQueryIndexedEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdEndQueryIndexedEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdEndQueryIndexedEXT :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
cmdEndQueryIndexedEXT = \(CommandBuffer commandBuffer' commandTable) -> \queryPool' -> \query' -> \index' -> vkCmdEndQueryIndexedEXT commandTable commandBuffer' queryPool' query' index' *> (pure ())


-- | vkCmdEndTransformFeedbackEXT - Make transform feedback inactive in the
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstCounterBuffer@ is the index of the first transform feedback
--     buffer corresponding to @pCounterBuffers@[0] and
--     @pCounterBufferOffsets@[0].
--
-- -   @counterBufferCount@ is the size of the @pCounterBuffers@ and
--     @pCounterBufferOffsets@ arrays.
--
-- -   @pCounterBuffers@ is an optional array of buffer handles to the
--     counter buffers used to record the current byte positions of each
--     transform feedback buffer where the next vertex output data would be
--     captured. This /can/ be used by a subsequent
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBeginTransformFeedbackEXT'
--     call to resume transform feedback capture from this position. It can
--     also be used by
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdDrawIndirectByteCountEXT'
--     to determine the vertex count of the draw call.
--
-- -   @pCounterBufferOffsets@ is an optional array of offsets within each
--     of the @pCounterBuffers@ where the counter values can be written.
--     The location in each counter buffer at these offsets /must/ be large
--     enough to contain 4 bytes of data. The data stored at this location
--     is the byte offset from the start of the transform feedback buffer
--     binding where the next vertex data would be written. If
--     @pCounterBufferOffsets@ is @NULL@, then it is assumed the offsets
--     are zero.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   Transform feedback /must/ be active
--
-- -   @firstCounterBuffer@ /must/ be less than
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstCounterBuffer@ and @counterBufferCount@ /must/ be
--     less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBuffers@ is not
--     @NULL@, @pCounterBuffers@ /must/ be a valid pointer to an array of
--     @counterBufferCount@
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handles that
--     are either valid or
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   For each buffer handle in the array, if it is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/
--     reference a buffer large enough to hold 4 bytes at the corresponding
--     offset from the @pCounterBufferOffsets@ array
--
-- -   If @pCounterBuffer@ is @NULL@, then @pCounterBufferOffsets@ /must/
--     also be @NULL@
--
-- -   For each buffer handle in the @pCounterBuffers@ array that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ have
--     been created with a @usage@ value containing
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- Unresolved directive in vkCmdEndTransformFeedbackEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdEndTransformFeedbackEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdEndTransformFeedbackEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Maybe (Vector DeviceSize) ->  IO ()
cmdEndTransformFeedbackEXT = \(CommandBuffer commandBuffer' commandTable) -> \firstCounterBuffer' -> \counterBuffers' -> \counterBufferOffsets' -> maybeWith (withVec (&)) counterBufferOffsets' (\pCounterBufferOffsets' -> withVec (&) counterBuffers' (\pCounterBuffers' -> vkCmdEndTransformFeedbackEXT commandTable commandBuffer' firstCounterBuffer' (fromIntegral $ Data.Vector.length counterBuffers' `min` maybe maxBound Data.Vector.length counterBufferOffsets') pCounterBuffers' pCounterBufferOffsets' *> (pure ())))
