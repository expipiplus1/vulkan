{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_transform_feedback
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , 
  PhysicalDeviceTransformFeedbackPropertiesEXT(..)
#endif
  , PipelineRasterizationStateStreamCreateFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineRasterizationStateStreamCreateInfoEXT(..)
#endif
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
  ( maybeWith
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( VkPipelineRasterizationStateStreamCreateFlagsEXT(..)
  , vkCmdBeginQueryIndexedEXT
  , vkCmdBeginTransformFeedbackEXT
  , vkCmdBindTransformFeedbackBuffersEXT
  , vkCmdDrawIndirectByteCountEXT
  , vkCmdEndQueryIndexedEXT
  , vkCmdEndTransformFeedbackEXT
  , pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.CommandBuffer
  ( QueryControlFlags
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

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
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



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceTransformFeedbackFeaturesEXT"
data PhysicalDeviceTransformFeedbackFeaturesEXT = PhysicalDeviceTransformFeedbackFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "transformFeedback"
  transformFeedback :: Bool
  , -- No documentation found for Nested "PhysicalDeviceTransformFeedbackFeaturesEXT" "geometryStreams"
  geometryStreams :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceTransformFeedbackFeaturesEXT where
  zero = PhysicalDeviceTransformFeedbackFeaturesEXT Nothing
                                                    False
                                                    False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceTransformFeedbackPropertiesEXT"
data PhysicalDeviceTransformFeedbackPropertiesEXT = PhysicalDeviceTransformFeedbackPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceTransformFeedbackPropertiesEXT" "pNext"
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

#endif

-- No documentation found for TopLevel "PipelineRasterizationStateStreamCreateFlagsEXT"
type PipelineRasterizationStateStreamCreateFlagsEXT = VkPipelineRasterizationStateStreamCreateFlagsEXT


-- No complete pragma for PipelineRasterizationStateStreamCreateFlagsEXT as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineRasterizationStateStreamCreateInfoEXT"
data PipelineRasterizationStateStreamCreateInfoEXT = PipelineRasterizationStateStreamCreateInfoEXT
  { -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "flags"
  flags :: PipelineRasterizationStateStreamCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationStateStreamCreateInfoEXT" "rasterizationStream"
  rasterizationStream :: Word32
  }
  deriving (Show, Eq)

instance Zero PipelineRasterizationStateStreamCreateInfoEXT where
  zero = PipelineRasterizationStateStreamCreateInfoEXT Nothing
                                                       zero
                                                       zero

#endif


-- No documentation found for TopLevel "vkCmdBeginQueryIndexedEXT"
cmdBeginQueryIndexedEXT :: CommandBuffer ->  QueryPool ->  Word32 ->  QueryControlFlags ->  Word32 ->  IO ()
cmdBeginQueryIndexedEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBeginTransformFeedbackEXT"
cmdBeginTransformFeedbackEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Maybe (Vector DeviceSize) ->  IO ()
cmdBeginTransformFeedbackEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBindTransformFeedbackBuffersEXT"
cmdBindTransformFeedbackBuffersEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Vector DeviceSize ->  Maybe (Vector DeviceSize) ->  IO ()
cmdBindTransformFeedbackBuffersEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDrawIndirectByteCountEXT"
cmdDrawIndirectByteCountEXT :: CommandBuffer ->  Word32 ->  Word32 ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirectByteCountEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdEndQueryIndexedEXT"
cmdEndQueryIndexedEXT :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
cmdEndQueryIndexedEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdEndTransformFeedbackEXT"
cmdEndTransformFeedbackEXT :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Maybe (Vector DeviceSize) ->  IO ()
cmdEndTransformFeedbackEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME"
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION"
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION :: Integral a => a
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
