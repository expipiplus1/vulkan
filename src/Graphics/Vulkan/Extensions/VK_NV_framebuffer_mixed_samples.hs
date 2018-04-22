{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkCoverageModulationModeNV(..)
  , pattern VK_COVERAGE_MODULATION_MODE_NONE_NV
  , pattern VK_COVERAGE_MODULATION_MODE_RGB_NV
  , pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV
  , pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV
  , VkPipelineCoverageModulationStateCreateFlagsNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  , VkPipelineCoverageModulationStateCreateInfoNV(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
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


-- ** VkCoverageModulationModeNV

-- | VkCoverageModulationModeNV - Specify the discard rectangle mode
--
-- = See Also
--
-- 'VkPipelineCoverageModulationStateCreateInfoNV'
newtype VkCoverageModulationModeNV = VkCoverageModulationModeNV Int32
  deriving (Eq, Ord, Storable)

instance Show VkCoverageModulationModeNV where
  showsPrec _ VK_COVERAGE_MODULATION_MODE_NONE_NV = showString "VK_COVERAGE_MODULATION_MODE_NONE_NV"
  showsPrec _ VK_COVERAGE_MODULATION_MODE_RGB_NV = showString "VK_COVERAGE_MODULATION_MODE_RGB_NV"
  showsPrec _ VK_COVERAGE_MODULATION_MODE_ALPHA_NV = showString "VK_COVERAGE_MODULATION_MODE_ALPHA_NV"
  showsPrec _ VK_COVERAGE_MODULATION_MODE_RGBA_NV = showString "VK_COVERAGE_MODULATION_MODE_RGBA_NV"
  showsPrec p (VkCoverageModulationModeNV x) = showParen (p >= 11) (showString "VkCoverageModulationModeNV " . showsPrec 11 x)

instance Read VkCoverageModulationModeNV where
  readPrec = parens ( choose [ ("VK_COVERAGE_MODULATION_MODE_NONE_NV",  pure VK_COVERAGE_MODULATION_MODE_NONE_NV)
                             , ("VK_COVERAGE_MODULATION_MODE_RGB_NV",   pure VK_COVERAGE_MODULATION_MODE_RGB_NV)
                             , ("VK_COVERAGE_MODULATION_MODE_ALPHA_NV", pure VK_COVERAGE_MODULATION_MODE_ALPHA_NV)
                             , ("VK_COVERAGE_MODULATION_MODE_RGBA_NV",  pure VK_COVERAGE_MODULATION_MODE_RGBA_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCoverageModulationModeNV")
                        v <- step readPrec
                        pure (VkCoverageModulationModeNV v)
                        )
                    )

-- | @VK_COVERAGE_MODULATION_MODE_NONE_NV@ specifies that no components are
-- multiplied by the modulation factor.
pattern VK_COVERAGE_MODULATION_MODE_NONE_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_NONE_NV = VkCoverageModulationModeNV 0

-- | @VK_COVERAGE_MODULATION_MODE_RGB_NV@ specifies that the red, green, and
-- blue components are multiplied by the modulation factor.
pattern VK_COVERAGE_MODULATION_MODE_RGB_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_RGB_NV = VkCoverageModulationModeNV 1

-- | @VK_COVERAGE_MODULATION_MODE_ALPHA_NV@ specifies that the alpha
-- component is multiplied by the modulation factor.
pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV = VkCoverageModulationModeNV 2

-- | @VK_COVERAGE_MODULATION_MODE_RGBA_NV@ specifies that all components are
-- multiplied by the modulation factor.
pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV = VkCoverageModulationModeNV 3
-- ** VkPipelineCoverageModulationStateCreateFlagsNV

-- | VkPipelineCoverageModulationStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- @VkPipelineCoverageModulationStateCreateFlagsNV@ is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'VkPipelineCoverageModulationStateCreateInfoNV'
newtype VkPipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineCoverageModulationStateCreateFlagsNV where
  
  showsPrec p (VkPipelineCoverageModulationStateCreateFlagsNV x) = showParen (p >= 11) (showString "VkPipelineCoverageModulationStateCreateFlagsNV " . showsPrec 11 x)

instance Read VkPipelineCoverageModulationStateCreateFlagsNV where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCoverageModulationStateCreateFlagsNV")
                        v <- step readPrec
                        pure (VkPipelineCoverageModulationStateCreateFlagsNV v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV = VkStructureType 1000152000
-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION"
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION :: Integral a => a
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME"
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = "VK_NV_framebuffer_mixed_samples"
-- | VkPipelineCoverageModulationStateCreateInfoNV - Structure specifying
-- parameters controlling coverage modulation
--
-- = Description
--
-- If @coverageModulationTableEnable@ is @VK_FALSE@, then for each color
-- sample the associated bits of the fragment’s coverage are counted and
-- divided by the number of associated bits to produce a modulation factor
-- R in the range (0,1] (a value of zero would have been killed due to a
-- color coverage of 0). Specifically:
--
-- -   N = value of @rasterizationSamples@
--
-- -   M = value of
--     'Graphics.Vulkan.Core10.Pass.VkAttachmentDescription'::@samples@ for
--     any color attachments
--
-- -   R = popcount(associated coverage bits) \/ (N \/ M)
--
-- If @coverageModulationTableEnable@ is @VK_TRUE@, the value R is computed
-- using a programmable lookup table. The lookup table has N \/ M elements,
-- and the element of the table is selected by:
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
-- If this structure is not present, it is as if coverageModulationMode is
-- @VK_COVERAGE_MODULATION_MODE_NONE_NV@.
--
-- == Valid Usage
--
-- -   If @coverageModulationTableEnable@ is @VK_TRUE@,
--     @coverageModulationTableCount@ /must/ be equal to the number of
--     rasterization samples divided by the number of color samples in the
--     subpass.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV@
--
-- -   @flags@ /must/ be @0@
--
-- -   @coverageModulationMode@ /must/ be a valid
--     'VkCoverageModulationModeNV' value
--
-- -   @coverageModulationTableCount@ /must/ be greater than @0@
--
-- = See Also
--
-- @VkBool32@, 'VkCoverageModulationModeNV',
-- 'VkPipelineCoverageModulationStateCreateFlagsNV',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineCoverageModulationStateCreateInfoNV = VkPipelineCoverageModulationStateCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineCoverageModulationStateCreateFlagsNV
  , -- | @coverageModulationMode@ controls which color components are modulated
  -- and is of type 'VkCoverageModulationModeNV'.
  vkCoverageModulationMode :: VkCoverageModulationModeNV
  , -- | @coverageModulationTableEnable@ controls whether the modulation factor
  -- is looked up from a table in @pCoverageModulationTable@.
  vkCoverageModulationTableEnable :: VkBool32
  , -- | @coverageModulationTableCount@ is the number of elements in
  -- @pCoverageModulationTable@.
  vkCoverageModulationTableCount :: Word32
  , -- | @pCoverageModulationTable@ is a table of modulation factors containing a
  -- value for each number of covered samples.
  vkPCoverageModulationTable :: Ptr CFloat
  }
  deriving (Eq, Show)

instance Storable VkPipelineCoverageModulationStateCreateInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPipelineCoverageModulationStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 20)
                                                           <*> peek (ptr `plusPtr` 24)
                                                           <*> peek (ptr `plusPtr` 28)
                                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkCoverageModulationMode (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkCoverageModulationTableEnable (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 28) (vkCoverageModulationTableCount (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 32) (vkPCoverageModulationTable (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
