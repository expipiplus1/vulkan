{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkCoverageModulationModeNV(..)
  , pattern VK_COVERAGE_MODULATION_MODE_NONE_NV
  , pattern VK_COVERAGE_MODULATION_MODE_RGB_NV
  , pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV
  , pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV
  , VkPipelineCoverageModulationStateCreateFlagsNV(..)
  , VkPipelineCoverageModulationStateCreateInfoNV(..)
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )


-- ** VkCoverageModulationModeNV

-- No documentation found for TopLevel "VkCoverageModulationModeNV"
newtype VkCoverageModulationModeNV = VkCoverageModulationModeNV Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for Nested "VkCoverageModulationModeNV" "VK_COVERAGE_MODULATION_MODE_NONE_NV"
pattern VK_COVERAGE_MODULATION_MODE_NONE_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_NONE_NV = VkCoverageModulationModeNV 0

-- No documentation found for Nested "VkCoverageModulationModeNV" "VK_COVERAGE_MODULATION_MODE_RGB_NV"
pattern VK_COVERAGE_MODULATION_MODE_RGB_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_RGB_NV = VkCoverageModulationModeNV 1

-- No documentation found for Nested "VkCoverageModulationModeNV" "VK_COVERAGE_MODULATION_MODE_ALPHA_NV"
pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV = VkCoverageModulationModeNV 2

-- No documentation found for Nested "VkCoverageModulationModeNV" "VK_COVERAGE_MODULATION_MODE_RGBA_NV"
pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV = VkCoverageModulationModeNV 3

-- ** VkPipelineCoverageModulationStateCreateFlagsNV

-- No documentation found for TopLevel "VkPipelineCoverageModulationStateCreateFlagsNV"
newtype VkPipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "VkPipelineCoverageModulationStateCreateInfoNV"
data VkPipelineCoverageModulationStateCreateInfoNV = VkPipelineCoverageModulationStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "flags"
  vkFlags :: VkPipelineCoverageModulationStateCreateFlagsNV
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "coverageModulationMode"
  vkCoverageModulationMode :: VkCoverageModulationModeNV
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "coverageModulationTableEnable"
  vkCoverageModulationTableEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "coverageModulationTableCount"
  vkCoverageModulationTableCount :: Word32
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "pCoverageModulationTable"
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

instance Zero VkPipelineCoverageModulationStateCreateInfoNV where
  zero = VkPipelineCoverageModulationStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero

-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME"
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = "VK_NV_framebuffer_mixed_samples"

-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION"
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION :: Integral a => a
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV = VkStructureType 1000152000
