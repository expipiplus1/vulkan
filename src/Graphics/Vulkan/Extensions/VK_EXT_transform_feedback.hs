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
  , pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
  , pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
  , pattern QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
  , pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  , pattern ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
  , pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
  , pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
  , pattern PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.String
  ( IsString
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
  , pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Buffer
  ( pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
  , pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
  , pattern ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
  )
import Graphics.Vulkan.Core10.Query
  ( pattern QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
  )
import Graphics.Vulkan.Core10.Queue
  ( pattern PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPipelineRasterizationStateStreamCreateInfoEXT'
type PipelineRasterizationStateStreamCreateFlagsEXT = VkPipelineRasterizationStateStreamCreateFlagsEXT


-- No complete pragma for PipelineRasterizationStateStreamCreateFlagsEXT as it has no patterns


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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPipelineRasterizationStateStreamCreateFlagsEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits'
--     values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlags',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool'
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBufferOffsets@ is
--     not @NULL@, @pCounterBufferOffsets@ /must/ be a valid pointer to an
--     array of @counterBufferCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize' values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @commandBuffer@, and the elements of @pCounterBuffers@ that
--     are valid handles /must/ have been created, allocated, or retrieved
--     from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pBuffers@ /must/ be a valid pointer to an array of @bindingCount@
--     valid 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handles
--
-- -   @pOffsets@ /must/ be a valid pointer to an array of @bindingCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize' values
--
-- -   If @pSizes@ is not @NULL@, @pSizes@ /must/ be a valid pointer to an
--     array of @bindingCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize' values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If @pSizes@ is not @NULL@, @bindingCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pBuffers@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
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
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   The implementation /must/ support
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackDraw@
--
-- -   @vertexStride@ /must/ be greater than 0 and less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxTransformFeedbackBufferDataStride@
--
-- -   @counterBuffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @counterBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @commandBuffer@, and @counterBuffer@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
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
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ parameter /must/ be less than
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   If the @queryType@ used to create @queryPool@ was not
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ /must/ be zero
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool'
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBufferOffsets@ is
--     not @NULL@, @pCounterBufferOffsets@ /must/ be a valid pointer to an
--     array of @counterBufferCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize' values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @commandBuffer@, and the elements of @pCounterBuffers@ that
--     are valid handles /must/ have been created, allocated, or retrieved
--     from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdEndTransformFeedbackEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Maybe (Vector DeviceSize) ->  IO ()
cmdEndTransformFeedbackEXT = \(CommandBuffer commandBuffer' commandTable) -> \firstCounterBuffer' -> \counterBuffers' -> \counterBufferOffsets' -> maybeWith (withVec (&)) counterBufferOffsets' (\pCounterBufferOffsets' -> withVec (&) counterBuffers' (\pCounterBuffers' -> vkCmdEndTransformFeedbackEXT commandTable commandBuffer' firstCounterBuffer' (fromIntegral $ Data.Vector.length counterBuffers' `min` maybe maxBound Data.Vector.length counterBufferOffsets') pCounterBuffers' pCounterBufferOffsets' *> (pure ())))

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME"
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION"
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION :: Integral a => a
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
