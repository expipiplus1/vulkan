{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  ( PipelineCoverageToColorStateCreateFlagsNV
  , withCStructPipelineCoverageToColorStateCreateInfoNV
  , fromCStructPipelineCoverageToColorStateCreateInfoNV
  , PipelineCoverageToColorStateCreateInfoNV(..)
  , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  ) where

import Data.Maybe
  ( fromMaybe
  , maybe
  )
import Data.String
  ( IsString
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
  , pattern VK_FALSE
  , pattern VK_TRUE
  )
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateFlagsNV(..)
  , VkPipelineCoverageToColorStateCreateInfoNV(..)
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  )


-- | VkPipelineCoverageToColorStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color.VkPipelineCoverageToColorStateCreateFlagsNV'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color.VkPipelineCoverageToColorStateCreateInfoNV'
type PipelineCoverageToColorStateCreateFlagsNV = VkPipelineCoverageToColorStateCreateFlagsNV


-- No complete pragma for PipelineCoverageToColorStateCreateFlagsNV as it has no patterns


-- | VkPipelineCoverageToColorStateCreateInfoNV - Structure specifying
-- whether fragment coverage replaces a color
--
-- = Description
--
-- If @coverageToColorEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
-- the fragment coverage information is treated as a bitmask with one bit
-- for each sample (as in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-samplemask Sample Mask>
-- section), and this bitmask replaces the first component of the color
-- value corresponding to the fragment shader output location with
-- @Location@ equal to @coverageToColorLocation@ and @Index@ equal to zero.
-- If the color attachment format has fewer bits than the sample coverage,
-- the low bits of the sample coverage bitmask are taken without any
-- clamping. If the color attachment format has more bits than the sample
-- coverage, the high bits of the sample coverage bitmask are filled with
-- zeros.
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-sampleshading Sample Shading>
-- is in use, the coverage bitmask only has bits set for samples that
-- correspond to the fragment shader invocation that shades those samples.
--
-- This pipeline stage occurs after sample counting and before blending,
-- and is always performed after fragment shading regardless of the setting
-- of @EarlyFragmentTests@.
--
-- If @coverageToColorEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE',
-- these operations are skipped. If this structure is not present, it is as
-- if @coverageToColorEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- == Valid Usage
--
-- -   If @coverageToColorEnable@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', then the render pass
--     subpass indicated by
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'::@renderPass@
--     and
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'::@subpass@
--     /must/ have a color attachment at the location selected by
--     @coverageToColorLocation@, with a
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_UINT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_SINT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_UINT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_SINT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32_UINT', or
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32_SINT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color.VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV'
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color.VkPipelineCoverageToColorStateCreateFlagsNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineCoverageToColorStateCreateInfoNV = PipelineCoverageToColorStateCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "flags"
  flags :: PipelineCoverageToColorStateCreateFlagsNV
  -- enable flag member elided
  , -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "coverageToColorLocation"
  coverageToColorLocation :: Maybe Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineCoverageToColorStateCreateInfoNV' and
-- marshal a 'PipelineCoverageToColorStateCreateInfoNV' into it. The 'VkPipelineCoverageToColorStateCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineCoverageToColorStateCreateInfoNV :: PipelineCoverageToColorStateCreateInfoNV -> (VkPipelineCoverageToColorStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineCoverageToColorStateCreateInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineCoverageToColorStateCreateInfoNV)) (\pPNext -> cont (VkPipelineCoverageToColorStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV pPNext (flags (marshalled :: PipelineCoverageToColorStateCreateInfoNV)) (maybe VK_FALSE (const VK_TRUE) (coverageToColorLocation (marshalled :: PipelineCoverageToColorStateCreateInfoNV))) (fromMaybe 0 (coverageToColorLocation (marshalled :: PipelineCoverageToColorStateCreateInfoNV)))))

-- | A function to read a 'VkPipelineCoverageToColorStateCreateInfoNV' and all additional
-- structures in the pointer chain into a 'PipelineCoverageToColorStateCreateInfoNV'.
fromCStructPipelineCoverageToColorStateCreateInfoNV :: VkPipelineCoverageToColorStateCreateInfoNV -> IO PipelineCoverageToColorStateCreateInfoNV
fromCStructPipelineCoverageToColorStateCreateInfoNV c = PipelineCoverageToColorStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineCoverageToColorStateCreateInfoNV)))
                                                                                                 <*> pure (vkFlags (c :: VkPipelineCoverageToColorStateCreateInfoNV))
                                                                                                 -- enable flag member elided
                                                                                                 <*> pure (let nz = (vkCoverageToColorLocation (c :: VkPipelineCoverageToColorStateCreateInfoNV)) in if nz == 0 then Nothing else Just nz)

instance Zero PipelineCoverageToColorStateCreateInfoNV where
  zero = PipelineCoverageToColorStateCreateInfoNV Nothing
                                                  zero
                                                  Nothing


-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME"
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION"
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION :: Integral a => a
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
