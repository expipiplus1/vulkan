{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( CoverageModulationModeNV
  , pattern COVERAGE_MODULATION_MODE_NONE_NV
  , pattern COVERAGE_MODULATION_MODE_RGB_NV
  , pattern COVERAGE_MODULATION_MODE_ALPHA_NV
  , pattern COVERAGE_MODULATION_MODE_RGBA_NV
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
  , pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV
  , pattern VK_COVERAGE_MODULATION_MODE_NONE_NV
  , pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV
  , pattern VK_COVERAGE_MODULATION_MODE_RGB_NV
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


-- | VkCoverageModulationModeNV - Specify the discard rectangle mode
--
-- = See Also
--
-- No cross-references are available
type CoverageModulationModeNV = VkCoverageModulationModeNV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VK_COVERAGE_MODULATION_MODE_NONE_NV'
-- specifies that no components are multiplied by the modulation factor.
pattern COVERAGE_MODULATION_MODE_NONE_NV :: (a ~ CoverageModulationModeNV) => a
pattern COVERAGE_MODULATION_MODE_NONE_NV = VK_COVERAGE_MODULATION_MODE_NONE_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VK_COVERAGE_MODULATION_MODE_RGB_NV'
-- specifies that the red, green, and blue components are multiplied by the
-- modulation factor.
pattern COVERAGE_MODULATION_MODE_RGB_NV :: (a ~ CoverageModulationModeNV) => a
pattern COVERAGE_MODULATION_MODE_RGB_NV = VK_COVERAGE_MODULATION_MODE_RGB_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VK_COVERAGE_MODULATION_MODE_ALPHA_NV'
-- specifies that the alpha component is multiplied by the modulation
-- factor.
pattern COVERAGE_MODULATION_MODE_ALPHA_NV :: (a ~ CoverageModulationModeNV) => a
pattern COVERAGE_MODULATION_MODE_ALPHA_NV = VK_COVERAGE_MODULATION_MODE_ALPHA_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VK_COVERAGE_MODULATION_MODE_RGBA_NV'
-- specifies that all components are multiplied by the modulation factor.
pattern COVERAGE_MODULATION_MODE_RGBA_NV :: (a ~ CoverageModulationModeNV) => a
pattern COVERAGE_MODULATION_MODE_RGBA_NV = VK_COVERAGE_MODULATION_MODE_RGBA_NV

-- | VkPipelineCoverageModulationStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VkPipelineCoverageModulationStateCreateFlagsNV'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- No cross-references are available
type PipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV


-- | VkPipelineCoverageModulationStateCreateInfoNV - Structure specifying
-- parameters controlling coverage modulation
--
-- = Description
--
-- If @coverageModulationTableEnable@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then for each color sample the
-- associated bits of the fragment’s coverage are counted and divided by
-- the number of associated bits to produce a modulation factor R in the
-- range (0,1] (a value of zero would have been killed due to a color
-- coverage of 0). Specifically:
--
-- -   N = value of @rasterizationSamples@
--
-- -   M = value of
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription'::@samples@
--     for any color attachments
--
-- -   R = popcount(associated coverage bits) \/ (N \/ M)
--
-- If @coverageModulationTableEnable@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the value R is computed using a
-- programmable lookup table. The lookup table has N \/ M elements, and the
-- element of the table is selected by:
--
-- -   R = @pCoverageModulationTable@[popcount(associated coverage bits)-1]
--
-- Note that the table does not have an entry for popcount(associated
-- coverage bits) = 0, because such samples would have been killed.
--
-- The values of @pCoverageModulationTable@ /may/ be rounded to an
-- implementation-dependent precision, which is at least as fine as 1 \/ N,
-- and clamped to [0,1].
--
-- For each color attachment with a floating point or normalized color
-- format, each fragment output color value is replicated to M values which
-- /can/ each be modulated (multiplied) by that color sample’s associated
-- value of R. Which components are modulated is controlled by
-- @coverageModulationMode@.
--
-- If this structure is not present, it is as if @coverageModulationMode@
-- is
-- 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VK_COVERAGE_MODULATION_MODE_NONE_NV'.
--
-- == Valid Usage
--
-- -   If @coverageModulationTableEnable@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     @coverageModulationTableCount@ /must/ be equal to the number of
--     rasterization samples divided by the number of color samples in the
--     subpass.
--
-- Unresolved directive in
-- VkPipelineCoverageModulationStateCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkPipelineCoverageModulationStateCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineCoverageModulationStateCreateInfoNV = PipelineCoverageModulationStateCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "flags"
  flags :: PipelineCoverageModulationStateCreateFlagsNV
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "coverageModulationMode"
  coverageModulationMode :: CoverageModulationModeNV
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "coverageModulationTableEnable"
  coverageModulationTableEnable :: Bool
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "pCoverageModulationTable"
  coverageModulationTable :: Maybe (Vector CFloat)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineCoverageModulationStateCreateInfoNV' and
-- marshal a 'PipelineCoverageModulationStateCreateInfoNV' into it. The 'VkPipelineCoverageModulationStateCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineCoverageModulationStateCreateInfoNV :: PipelineCoverageModulationStateCreateInfoNV -> (VkPipelineCoverageModulationStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineCoverageModulationStateCreateInfoNV marshalled cont = maybeWith (withVec (&)) (coverageModulationTable (marshalled :: PipelineCoverageModulationStateCreateInfoNV)) (\pPCoverageModulationTable -> maybeWith withSomeVkStruct (next (marshalled :: PipelineCoverageModulationStateCreateInfoNV)) (\pPNext -> cont (VkPipelineCoverageModulationStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV pPNext (flags (marshalled :: PipelineCoverageModulationStateCreateInfoNV)) (coverageModulationMode (marshalled :: PipelineCoverageModulationStateCreateInfoNV)) (boolToBool32 (coverageModulationTableEnable (marshalled :: PipelineCoverageModulationStateCreateInfoNV))) (maybe 0 (fromIntegral . Data.Vector.length) (coverageModulationTable (marshalled :: PipelineCoverageModulationStateCreateInfoNV))) pPCoverageModulationTable)))

-- | A function to read a 'VkPipelineCoverageModulationStateCreateInfoNV' and all additional
-- structures in the pointer chain into a 'PipelineCoverageModulationStateCreateInfoNV'.
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

