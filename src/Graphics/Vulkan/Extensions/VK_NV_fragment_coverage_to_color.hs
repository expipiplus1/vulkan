{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateFlagsNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  , VkPipelineCoverageToColorStateCreateInfoNV(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkFlags
  )


-- ** VkPipelineCoverageToColorStateCreateFlagsNV

-- | VkPipelineCoverageToColorStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- @VkPipelineCoverageToColorStateCreateFlagsNV@ is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'VkPipelineCoverageToColorStateCreateInfoNV'
newtype VkPipelineCoverageToColorStateCreateFlagsNV = VkPipelineCoverageToColorStateCreateFlagsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineCoverageToColorStateCreateFlagsNV where
  
  showsPrec p (VkPipelineCoverageToColorStateCreateFlagsNV x) = showParen (p >= 11) (showString "VkPipelineCoverageToColorStateCreateFlagsNV " . showsPrec 11 x)

instance Read VkPipelineCoverageToColorStateCreateFlagsNV where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCoverageToColorStateCreateFlagsNV")
                        v <- step readPrec
                        pure (VkPipelineCoverageToColorStateCreateFlagsNV v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV = VkStructureType 1000149000
-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION"
pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION :: Integral a => a
pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME"
pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = "VK_NV_fragment_coverage_to_color"
-- | VkPipelineCoverageToColorStateCreateInfoNV - Structure specifying
-- whether fragment coverage replaces a color
--
-- = Description
--
-- If @coverageToColorEnable@ is @VK_TRUE@, the fragment coverage
-- information is treated as a bitmask with one bit for each sample (as in
-- the [Sample
-- Mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-samplemask)
-- section), and this bitmask replaces the first component of the color
-- value corresponding to the fragment shader output location with
-- @Location@ equal to @coverageToColorLocation@ and @Index@ equal to zero.
-- If the color attachment format has fewer bits than the sample coverage,
-- the low bits of the sample coverage bitmask are taken without any
-- clamping. If the color attachment format has more bits than the sample
-- coverage, the high bits of the sample coverage bitmask are filled with
-- zeros.
--
-- If [Sample
-- Shading](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast-sampleshading)
-- is in use, the coverage bitmask only has bits set for samples that
-- correspond to the fragment shader invocation that shades those samples.
--
-- This pipeline stage occurs after sample counting and before blending,
-- and is always performed after fragment shading regardless of the setting
-- of @EarlyFragmentTests@.
--
-- If @coverageToColorEnable@ is @VK_FALSE@, these operations are skipped.
-- If this structure is not present, it is as if @coverageToColorEnable@ is
-- @VK_FALSE@.
--
-- == Valid Usage
--
-- -   If @coverageToColorEnable@ is @VK_TRUE@, then the render pass
--     subpass indicated by
--     'Graphics.Vulkan.Core10.Pipeline.VkGraphicsPipelineCreateInfo'::@renderPass@
--     and
--     'Graphics.Vulkan.Core10.Pipeline.VkGraphicsPipelineCreateInfo'::@subpass@
--     /must/ have a color attachment at the location selected by
--     @coverageToColorLocation@, with a
--     'Graphics.Vulkan.Core10.Core.VkFormat' of @VK_FORMAT_R8_UINT@,
--     @VK_FORMAT_R8_SINT@, @VK_FORMAT_R16_UINT@, @VK_FORMAT_R16_SINT@,
--     @VK_FORMAT_R32_UINT@, or @VK_FORMAT_R32_SINT@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- @VkBool32@, 'VkPipelineCoverageToColorStateCreateFlagsNV',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineCoverageToColorStateCreateInfoNV = VkPipelineCoverageToColorStateCreateInfoNV
  { -- | @sType@ is the type of this structure
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineCoverageToColorStateCreateFlagsNV
  , -- | @coverageToColorEnable@ controls whether the fragment coverage value
  -- replaces a fragment color output.
  vkCoverageToColorEnable :: VkBool32
  , -- | @coverageToColorLocation@ controls which fragment shader color output
  -- value is replaced.
  vkCoverageToColorLocation :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPipelineCoverageToColorStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineCoverageToColorStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
                                                        <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkCoverageToColorEnable (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkCoverageToColorLocation (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
