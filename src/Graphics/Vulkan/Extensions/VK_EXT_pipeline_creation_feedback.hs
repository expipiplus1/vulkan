{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PipelineCreationFeedbackCreateInfoEXT(..)
  , 
#endif
  PipelineCreationFeedbackEXT(..)
  , PipelineCreationFeedbackFlagBitsEXT
  , pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
  , pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
  , pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
  , PipelineCreationFeedbackFlagsEXT
  , pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
  , pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackFlagBitsEXT(..)
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
  , pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
  , pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
  , pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineCreationFeedbackCreateInfoEXT"
data PipelineCreationFeedbackCreateInfoEXT = PipelineCreationFeedbackCreateInfoEXT
  { -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pPipelineCreationFeedback"
  pipelineCreationFeedback :: Ptr VkPipelineCreationFeedbackEXT
  , -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pipelineStageCreationFeedbackCount"
  pipelineStageCreationFeedbackCount :: Word32
  , -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pPipelineStageCreationFeedbacks"
  pipelineStageCreationFeedbacks :: Ptr VkPipelineCreationFeedbackEXT
  }
  deriving (Show, Eq)

instance Zero PipelineCreationFeedbackCreateInfoEXT where
  zero = PipelineCreationFeedbackCreateInfoEXT Nothing
                                               nullPtr
                                               zero
                                               nullPtr

#endif


-- No documentation found for TopLevel "VkPipelineCreationFeedbackEXT"
data PipelineCreationFeedbackEXT = PipelineCreationFeedbackEXT
  { -- No documentation found for Nested "PipelineCreationFeedbackEXT" "flags"
  flags :: PipelineCreationFeedbackFlagsEXT
  , -- No documentation found for Nested "PipelineCreationFeedbackEXT" "duration"
  duration :: Word64
  }
  deriving (Show, Eq)

instance Zero PipelineCreationFeedbackEXT where
  zero = PipelineCreationFeedbackEXT zero
                                     zero


-- No documentation found for TopLevel "PipelineCreationFeedbackFlagBitsEXT"
type PipelineCreationFeedbackFlagBitsEXT = VkPipelineCreationFeedbackFlagBitsEXT


{-# complete PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT, PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT, PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT :: PipelineCreationFeedbackFlagBitsEXT #-}


-- No documentation found for Nested "PipelineCreationFeedbackFlagBitsEXT" "PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT :: (a ~ PipelineCreationFeedbackFlagBitsEXT) => a
pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT


-- No documentation found for Nested "PipelineCreationFeedbackFlagBitsEXT" "PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT :: (a ~ PipelineCreationFeedbackFlagBitsEXT) => a
pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT


-- No documentation found for Nested "PipelineCreationFeedbackFlagBitsEXT" "PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT :: (a ~ PipelineCreationFeedbackFlagBitsEXT) => a
pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT

-- No documentation found for TopLevel "PipelineCreationFeedbackFlagsEXT"
type PipelineCreationFeedbackFlagsEXT = PipelineCreationFeedbackFlagBitsEXT

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME"
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION"
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION :: Integral a => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
