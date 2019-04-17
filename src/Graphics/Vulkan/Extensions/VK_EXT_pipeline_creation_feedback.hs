{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback
  ( withCStructPipelineCreationFeedbackCreateInfoEXT
  , fromCStructPipelineCreationFeedbackCreateInfoEXT
  , PipelineCreationFeedbackCreateInfoEXT(..)
  , withCStructPipelineCreationFeedbackEXT
  , fromCStructPipelineCreationFeedbackEXT
  , PipelineCreationFeedbackEXT(..)
  , PipelineCreationFeedbackFlagBitsEXT
  , PipelineCreationFeedbackFlagsEXT
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word64
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackCreateInfoEXT(..)
  , VkPipelineCreationFeedbackEXT(..)
  , VkPipelineCreationFeedbackFlagBitsEXT(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
  )


-- No documentation found for TopLevel "PipelineCreationFeedbackCreateInfoEXT"
data PipelineCreationFeedbackCreateInfoEXT = PipelineCreationFeedbackCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pPipelineCreationFeedback"
  vkPPipelineCreationFeedback :: PipelineCreationFeedbackEXT
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pPipelineStageCreationFeedbacks"
  vkPPipelineStageCreationFeedbacks :: Vector PipelineCreationFeedbackEXT
  }
  deriving (Show, Eq)
withCStructPipelineCreationFeedbackCreateInfoEXT :: PipelineCreationFeedbackCreateInfoEXT -> (VkPipelineCreationFeedbackCreateInfoEXT -> IO a) -> IO a
withCStructPipelineCreationFeedbackCreateInfoEXT from cont = withVec withCStructPipelineCreationFeedbackEXT (vkPPipelineStageCreationFeedbacks (from :: PipelineCreationFeedbackCreateInfoEXT)) (\pPipelineStageCreationFeedbacks -> (\a -> withCStructPipelineCreationFeedbackEXT a . flip with) (vkPPipelineCreationFeedback (from :: PipelineCreationFeedbackCreateInfoEXT)) (\pPipelineCreationFeedback -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineCreationFeedbackCreateInfoEXT)) (\pPNext -> cont (VkPipelineCreationFeedbackCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT pPNext pPipelineCreationFeedback (fromIntegral (Data.Vector.length (vkPPipelineStageCreationFeedbacks (from :: PipelineCreationFeedbackCreateInfoEXT)))) pPipelineStageCreationFeedbacks))))
fromCStructPipelineCreationFeedbackCreateInfoEXT :: VkPipelineCreationFeedbackCreateInfoEXT -> IO PipelineCreationFeedbackCreateInfoEXT
fromCStructPipelineCreationFeedbackCreateInfoEXT c = PipelineCreationFeedbackCreateInfoEXT <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineCreationFeedbackCreateInfoEXT)))
                                                                                           <*> (fromCStructPipelineCreationFeedbackEXT <=< peek) (vkPPipelineCreationFeedback (c :: VkPipelineCreationFeedbackCreateInfoEXT))
                                                                                           -- Length valued member elided
                                                                                           <*> (Data.Vector.generateM (fromIntegral (vkPipelineStageCreationFeedbackCount (c :: VkPipelineCreationFeedbackCreateInfoEXT))) (((fromCStructPipelineCreationFeedbackEXT <=<) . peekElemOff) (vkPPipelineStageCreationFeedbacks (c :: VkPipelineCreationFeedbackCreateInfoEXT))))
instance Zero PipelineCreationFeedbackCreateInfoEXT where
  zero = PipelineCreationFeedbackCreateInfoEXT Nothing
                                               zero
                                               Data.Vector.empty
-- No documentation found for TopLevel "PipelineCreationFeedbackEXT"
data PipelineCreationFeedbackEXT = PipelineCreationFeedbackEXT
  { -- No documentation found for Nested "PipelineCreationFeedbackEXT" "flags"
  vkFlags :: PipelineCreationFeedbackFlagsEXT
  , -- No documentation found for Nested "PipelineCreationFeedbackEXT" "duration"
  vkDuration :: Word64
  }
  deriving (Show, Eq)
withCStructPipelineCreationFeedbackEXT :: PipelineCreationFeedbackEXT -> (VkPipelineCreationFeedbackEXT -> IO a) -> IO a
withCStructPipelineCreationFeedbackEXT from cont = cont (VkPipelineCreationFeedbackEXT (vkFlags (from :: PipelineCreationFeedbackEXT)) (vkDuration (from :: PipelineCreationFeedbackEXT)))
fromCStructPipelineCreationFeedbackEXT :: VkPipelineCreationFeedbackEXT -> IO PipelineCreationFeedbackEXT
fromCStructPipelineCreationFeedbackEXT c = PipelineCreationFeedbackEXT <$> pure (vkFlags (c :: VkPipelineCreationFeedbackEXT))
                                                                       <*> pure (vkDuration (c :: VkPipelineCreationFeedbackEXT))
instance Zero PipelineCreationFeedbackEXT where
  zero = PipelineCreationFeedbackEXT zero
                                     zero
-- No documentation found for TopLevel "PipelineCreationFeedbackFlagBitsEXT"
type PipelineCreationFeedbackFlagBitsEXT = VkPipelineCreationFeedbackFlagBitsEXT
-- No documentation found for TopLevel "PipelineCreationFeedbackFlagsEXT"
type PipelineCreationFeedbackFlagsEXT = PipelineCreationFeedbackFlagBitsEXT
