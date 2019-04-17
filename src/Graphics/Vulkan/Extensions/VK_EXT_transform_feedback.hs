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
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdBeginQueryIndexedEXT
  , cmdBeginTransformFeedbackEXT
  , cmdBindTransformFeedbackBuffersEXT
  , cmdDrawIndirectByteCountEXT
  , cmdEndQueryIndexedEXT
  , cmdEndTransformFeedbackEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( VkPhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , VkPhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , VkPipelineRasterizationStateStreamCreateFlagsEXT(..)
  , VkPipelineRasterizationStateStreamCreateInfoEXT(..)
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


-- No documentation found for TopLevel "PhysicalDeviceTransformFeedbackFeaturesEXT"
data PhysicalDeviceTransformFeedbackFeaturesEXT = PhysicalDeviceTransformFeedbackFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "transformFeedback"
  vkTransformFeedback :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "geometryStreams"
  vkGeometryStreams :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceTransformFeedbackFeaturesEXT :: PhysicalDeviceTransformFeedbackFeaturesEXT -> (VkPhysicalDeviceTransformFeedbackFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceTransformFeedbackFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceTransformFeedbackFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceTransformFeedbackFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT pPNext (boolToBool32 (vkTransformFeedback (from :: PhysicalDeviceTransformFeedbackFeaturesEXT))) (boolToBool32 (vkGeometryStreams (from :: PhysicalDeviceTransformFeedbackFeaturesEXT)))))
fromCStructPhysicalDeviceTransformFeedbackFeaturesEXT :: VkPhysicalDeviceTransformFeedbackFeaturesEXT -> IO PhysicalDeviceTransformFeedbackFeaturesEXT
fromCStructPhysicalDeviceTransformFeedbackFeaturesEXT c = PhysicalDeviceTransformFeedbackFeaturesEXT <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceTransformFeedbackFeaturesEXT)))
                                                                                                     <*> pure (bool32ToBool (vkTransformFeedback (c :: VkPhysicalDeviceTransformFeedbackFeaturesEXT)))
                                                                                                     <*> pure (bool32ToBool (vkGeometryStreams (c :: VkPhysicalDeviceTransformFeedbackFeaturesEXT)))
instance Zero PhysicalDeviceTransformFeedbackFeaturesEXT where
  zero = PhysicalDeviceTransformFeedbackFeaturesEXT Nothing
                                                    False
                                                    False
-- No documentation found for TopLevel "PhysicalDeviceTransformFeedbackPropertiesEXT"
data PhysicalDeviceTransformFeedbackPropertiesEXT = PhysicalDeviceTransformFeedbackPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackStreams"
  vkMaxTransformFeedbackStreams :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBuffers"
  vkMaxTransformFeedbackBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferSize"
  vkMaxTransformFeedbackBufferSize :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackStreamDataSize"
  vkMaxTransformFeedbackStreamDataSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferDataSize"
  vkMaxTransformFeedbackBufferDataSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferDataStride"
  vkMaxTransformFeedbackBufferDataStride :: Word32
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackQueries"
  vkTransformFeedbackQueries :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackStreamsLinesTriangles"
  vkTransformFeedbackStreamsLinesTriangles :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackRasterizationStreamSelect"
  vkTransformFeedbackRasterizationStreamSelect :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackDraw"
  vkTransformFeedbackDraw :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceTransformFeedbackPropertiesEXT :: PhysicalDeviceTransformFeedbackPropertiesEXT -> (VkPhysicalDeviceTransformFeedbackPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceTransformFeedbackPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceTransformFeedbackPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT pPNext (vkMaxTransformFeedbackStreams (from :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (vkMaxTransformFeedbackBuffers (from :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (vkMaxTransformFeedbackBufferSize (from :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (vkMaxTransformFeedbackStreamDataSize (from :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (vkMaxTransformFeedbackBufferDataSize (from :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (vkMaxTransformFeedbackBufferDataStride (from :: PhysicalDeviceTransformFeedbackPropertiesEXT)) (boolToBool32 (vkTransformFeedbackQueries (from :: PhysicalDeviceTransformFeedbackPropertiesEXT))) (boolToBool32 (vkTransformFeedbackStreamsLinesTriangles (from :: PhysicalDeviceTransformFeedbackPropertiesEXT))) (boolToBool32 (vkTransformFeedbackRasterizationStreamSelect (from :: PhysicalDeviceTransformFeedbackPropertiesEXT))) (boolToBool32 (vkTransformFeedbackDraw (from :: PhysicalDeviceTransformFeedbackPropertiesEXT)))))
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
-- No documentation found for TopLevel "PipelineRasterizationStateStreamCreateFlagsEXT"
type PipelineRasterizationStateStreamCreateFlagsEXT = VkPipelineRasterizationStateStreamCreateFlagsEXT
-- No documentation found for TopLevel "PipelineRasterizationStateStreamCreateInfoEXT"
data PipelineRasterizationStateStreamCreateInfoEXT = PipelineRasterizationStateStreamCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "flags"
  vkFlags :: PipelineRasterizationStateStreamCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "rasterizationStream"
  vkRasterizationStream :: Word32
  }
  deriving (Show, Eq)
withCStructPipelineRasterizationStateStreamCreateInfoEXT :: PipelineRasterizationStateStreamCreateInfoEXT -> (VkPipelineRasterizationStateStreamCreateInfoEXT -> IO a) -> IO a
withCStructPipelineRasterizationStateStreamCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineRasterizationStateStreamCreateInfoEXT)) (\pPNext -> cont (VkPipelineRasterizationStateStreamCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT pPNext (vkFlags (from :: PipelineRasterizationStateStreamCreateInfoEXT)) (vkRasterizationStream (from :: PipelineRasterizationStateStreamCreateInfoEXT))))
fromCStructPipelineRasterizationStateStreamCreateInfoEXT :: VkPipelineRasterizationStateStreamCreateInfoEXT -> IO PipelineRasterizationStateStreamCreateInfoEXT
fromCStructPipelineRasterizationStateStreamCreateInfoEXT c = PipelineRasterizationStateStreamCreateInfoEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationStateStreamCreateInfoEXT)))
                                                                                                           <*> pure (vkFlags (c :: VkPipelineRasterizationStateStreamCreateInfoEXT))
                                                                                                           <*> pure (vkRasterizationStream (c :: VkPipelineRasterizationStateStreamCreateInfoEXT))
instance Zero PipelineRasterizationStateStreamCreateInfoEXT where
  zero = PipelineRasterizationStateStreamCreateInfoEXT Nothing
                                                       zero
                                                       zero

-- | Wrapper for 'vkCmdBeginQueryIndexedEXT'
cmdBeginQueryIndexedEXT :: CommandBuffer ->  QueryPool ->  Word32 ->  QueryControlFlags ->  Word32 ->  IO ()
cmdBeginQueryIndexedEXT = \(CommandBuffer commandBuffer commandTable) -> \queryPool -> \query -> \flags -> \index -> Graphics.Vulkan.C.Dynamic.cmdBeginQueryIndexedEXT commandTable commandBuffer queryPool query flags index *> (pure ())

-- | Wrapper for 'vkCmdBeginTransformFeedbackEXT'
cmdBeginTransformFeedbackEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Maybe (Vector DeviceSize) ->  IO ()
cmdBeginTransformFeedbackEXT = \(CommandBuffer commandBuffer commandTable) -> \firstCounterBuffer -> \counterBuffers -> \counterBufferOffsets -> maybeWith (withVec (&)) counterBufferOffsets (\pCounterBufferOffsets -> withVec (&) counterBuffers (\pCounterBuffers -> Graphics.Vulkan.C.Dynamic.cmdBeginTransformFeedbackEXT commandTable commandBuffer firstCounterBuffer (fromIntegral $ Data.Vector.length counterBuffers `min` maybe maxBound Data.Vector.length counterBufferOffsets) pCounterBuffers pCounterBufferOffsets *> (pure ())))

-- | Wrapper for 'vkCmdBindTransformFeedbackBuffersEXT'
cmdBindTransformFeedbackBuffersEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Vector DeviceSize ->  Maybe (Vector DeviceSize) ->  IO ()
cmdBindTransformFeedbackBuffersEXT = \(CommandBuffer commandBuffer commandTable) -> \firstBinding -> \buffers -> \offsets -> \sizes -> maybeWith (withVec (&)) sizes (\pSizes -> withVec (&) offsets (\pOffsets -> withVec (&) buffers (\pBuffers -> Graphics.Vulkan.C.Dynamic.cmdBindTransformFeedbackBuffersEXT commandTable commandBuffer firstBinding (fromIntegral $ Data.Vector.length buffers `min` Data.Vector.length offsets `min` maybe maxBound Data.Vector.length sizes) pBuffers pOffsets pSizes *> (pure ()))))

-- | Wrapper for 'vkCmdDrawIndirectByteCountEXT'
cmdDrawIndirectByteCountEXT :: CommandBuffer ->  Word32 ->  Word32 ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirectByteCountEXT = \(CommandBuffer commandBuffer commandTable) -> \instanceCount -> \firstInstance -> \counterBuffer -> \counterBufferOffset -> \counterOffset -> \vertexStride -> Graphics.Vulkan.C.Dynamic.cmdDrawIndirectByteCountEXT commandTable commandBuffer instanceCount firstInstance counterBuffer counterBufferOffset counterOffset vertexStride *> (pure ())

-- | Wrapper for 'vkCmdEndQueryIndexedEXT'
cmdEndQueryIndexedEXT :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
cmdEndQueryIndexedEXT = \(CommandBuffer commandBuffer commandTable) -> \queryPool -> \query -> \index -> Graphics.Vulkan.C.Dynamic.cmdEndQueryIndexedEXT commandTable commandBuffer queryPool query index *> (pure ())

-- | Wrapper for 'vkCmdEndTransformFeedbackEXT'
cmdEndTransformFeedbackEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Maybe (Vector DeviceSize) ->  IO ()
cmdEndTransformFeedbackEXT = \(CommandBuffer commandBuffer commandTable) -> \firstCounterBuffer -> \counterBuffers -> \counterBufferOffsets -> maybeWith (withVec (&)) counterBufferOffsets (\pCounterBufferOffsets -> withVec (&) counterBuffers (\pCounterBuffers -> Graphics.Vulkan.C.Dynamic.cmdEndTransformFeedbackEXT commandTable commandBuffer firstCounterBuffer (fromIntegral $ Data.Vector.length counterBuffers `min` maybe maxBound Data.Vector.length counterBufferOffsets) pCounterBuffers pCounterBufferOffsets *> (pure ())))
