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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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

-- | 
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

-- | 
pattern VK_COVERAGE_MODULATION_MODE_NONE_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_NONE_NV = VkCoverageModulationModeNV 0

-- | 
pattern VK_COVERAGE_MODULATION_MODE_RGB_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_RGB_NV = VkCoverageModulationModeNV 1

-- | 
pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV = VkCoverageModulationModeNV 2

-- | 
pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV :: VkCoverageModulationModeNV
pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV = VkCoverageModulationModeNV 3
-- ** VkPipelineCoverageModulationStateCreateFlagsNV

-- | 
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


-- | Nothing
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV = VkStructureType 1000152000
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION :: Integral a => a
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = "VK_NV_framebuffer_mixed_samples"
-- | TODO: Struct comments
data VkPipelineCoverageModulationStateCreateInfoNV = VkPipelineCoverageModulationStateCreateInfoNV
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkPipelineCoverageModulationStateCreateFlagsNV
  , vkCoverageModulationMode :: VkCoverageModulationModeNV
  , vkCoverageModulationTableEnable :: VkBool32
  , vkCoverageModulationTableCount :: Word32
  , vkCoverageModulationTable :: Ptr CFloat
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkCoverageModulationMode (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkCoverageModulationTableEnable (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 28) (vkCoverageModulationTableCount (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 32) (vkCoverageModulationTable (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
