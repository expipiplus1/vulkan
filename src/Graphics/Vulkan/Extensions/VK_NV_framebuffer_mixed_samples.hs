{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( CoverageModulationModeNV
  , PipelineCoverageModulationStateCreateFlagsNV
  , withCStructPipelineCoverageModulationStateCreateInfoNV
  , fromCStructPipelineCoverageModulationStateCreateInfoNV
  , PipelineCoverageModulationStateCreateInfoNV(..)
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  ) where

import Data.Function
  ( (&)
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkCoverageModulationModeNV(..)
  , VkPipelineCoverageModulationStateCreateFlagsNV(..)
  , VkPipelineCoverageModulationStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
  )


-- No documentation found for TopLevel "CoverageModulationModeNV"
type CoverageModulationModeNV = VkCoverageModulationModeNV
-- No documentation found for TopLevel "PipelineCoverageModulationStateCreateFlagsNV"
type PipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV
-- No documentation found for TopLevel "PipelineCoverageModulationStateCreateInfoNV"
data PipelineCoverageModulationStateCreateInfoNV = PipelineCoverageModulationStateCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "flags"
  vkFlags :: PipelineCoverageModulationStateCreateFlagsNV
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "coverageModulationMode"
  vkCoverageModulationMode :: CoverageModulationModeNV
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "coverageModulationTableEnable"
  vkCoverageModulationTableEnable :: Bool
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "pCoverageModulationTable"
  vkPCoverageModulationTable :: Maybe (Vector CFloat)
  }
  deriving (Show, Eq)
withCStructPipelineCoverageModulationStateCreateInfoNV :: PipelineCoverageModulationStateCreateInfoNV -> (VkPipelineCoverageModulationStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineCoverageModulationStateCreateInfoNV from cont = maybeWith (withVec (&)) (vkPCoverageModulationTable (from :: PipelineCoverageModulationStateCreateInfoNV)) (\pCoverageModulationTable -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineCoverageModulationStateCreateInfoNV)) (\pPNext -> cont (VkPipelineCoverageModulationStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV pPNext (vkFlags (from :: PipelineCoverageModulationStateCreateInfoNV)) (vkCoverageModulationMode (from :: PipelineCoverageModulationStateCreateInfoNV)) (boolToBool32 (vkCoverageModulationTableEnable (from :: PipelineCoverageModulationStateCreateInfoNV))) (maybe 0 (fromIntegral . Data.Vector.length) (vkPCoverageModulationTable (from :: PipelineCoverageModulationStateCreateInfoNV))) pCoverageModulationTable)))
fromCStructPipelineCoverageModulationStateCreateInfoNV :: VkPipelineCoverageModulationStateCreateInfoNV -> IO PipelineCoverageModulationStateCreateInfoNV
fromCStructPipelineCoverageModulationStateCreateInfoNV c = PipelineCoverageModulationStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineCoverageModulationStateCreateInfoNV)))
                                                                                                       <*> pure (vkFlags (c :: VkPipelineCoverageModulationStateCreateInfoNV))
                                                                                                       <*> pure (vkCoverageModulationMode (c :: VkPipelineCoverageModulationStateCreateInfoNV))
                                                                                                       <*> pure (bool32ToBool (vkCoverageModulationTableEnable (c :: VkPipelineCoverageModulationStateCreateInfoNV)))
                                                                                                       -- Optional length valued member elided
                                                                                                       <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkCoverageModulationTableCount (c :: VkPipelineCoverageModulationStateCreateInfoNV))) (peekElemOff p)) (vkPCoverageModulationTable (c :: VkPipelineCoverageModulationStateCreateInfoNV))
instance Zero PipelineCoverageModulationStateCreateInfoNV where
  zero = PipelineCoverageModulationStateCreateInfoNV Nothing
                                                     zero
                                                     zero
                                                     False
                                                     Nothing
