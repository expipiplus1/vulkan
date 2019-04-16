{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  ( PipelineCoverageToColorStateCreateFlagsNV
  , withCStructPipelineCoverageToColorStateCreateInfoNV
  , fromCStructPipelineCoverageToColorStateCreateInfoNV
  , PipelineCoverageToColorStateCreateInfoNV(..)
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  ) where

import Data.Maybe
  ( fromMaybe
  , maybe
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
  ( pattern VK_FALSE
  , pattern VK_TRUE
  )
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateFlagsNV(..)
  , VkPipelineCoverageToColorStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
  )


-- No documentation found for TopLevel "PipelineCoverageToColorStateCreateFlagsNV"
type PipelineCoverageToColorStateCreateFlagsNV = VkPipelineCoverageToColorStateCreateFlagsNV
-- No documentation found for TopLevel "PipelineCoverageToColorStateCreateInfoNV"
data PipelineCoverageToColorStateCreateInfoNV = PipelineCoverageToColorStateCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "flags"
  vkFlags :: PipelineCoverageToColorStateCreateFlagsNV
  -- enable flag member elided
  , -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "coverageToColorLocation"
  vkCoverageToColorLocation :: Maybe Word32
  }
  deriving (Show, Eq)
withCStructPipelineCoverageToColorStateCreateInfoNV :: PipelineCoverageToColorStateCreateInfoNV -> (VkPipelineCoverageToColorStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineCoverageToColorStateCreateInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineCoverageToColorStateCreateInfoNV)) (\pPNext -> cont (VkPipelineCoverageToColorStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV pPNext (vkFlags (from :: PipelineCoverageToColorStateCreateInfoNV)) (maybe VK_FALSE (const VK_TRUE) (vkCoverageToColorLocation (from :: PipelineCoverageToColorStateCreateInfoNV))) (fromMaybe 0 (vkCoverageToColorLocation (from :: PipelineCoverageToColorStateCreateInfoNV)))))
fromCStructPipelineCoverageToColorStateCreateInfoNV :: VkPipelineCoverageToColorStateCreateInfoNV -> IO PipelineCoverageToColorStateCreateInfoNV
fromCStructPipelineCoverageToColorStateCreateInfoNV c = PipelineCoverageToColorStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineCoverageToColorStateCreateInfoNV)))
                                                                                                 <*> pure (vkFlags (c :: VkPipelineCoverageToColorStateCreateInfoNV))
                                                                                                 -- enable flag member elided
                                                                                                 <*> pure (let x = (vkCoverageToColorLocation (c :: VkPipelineCoverageToColorStateCreateInfoNV)) in if x == 0 then Nothing else Just x)
