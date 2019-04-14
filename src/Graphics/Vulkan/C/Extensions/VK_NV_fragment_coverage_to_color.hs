{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateFlagsNV(..)
  , VkPipelineCoverageToColorStateCreateInfoNV(..)
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkFlags
  )


-- ** VkPipelineCoverageToColorStateCreateFlagsNV

-- No documentation found for TopLevel "VkPipelineCoverageToColorStateCreateFlagsNV"
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


-- No documentation found for TopLevel "VkPipelineCoverageToColorStateCreateInfoNV"
data VkPipelineCoverageToColorStateCreateInfoNV = VkPipelineCoverageToColorStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineCoverageToColorStateCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineCoverageToColorStateCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineCoverageToColorStateCreateInfoNV" "flags"
  vkFlags :: VkPipelineCoverageToColorStateCreateFlagsNV
  , -- No documentation found for Nested "VkPipelineCoverageToColorStateCreateInfoNV" "coverageToColorEnable"
  vkCoverageToColorEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineCoverageToColorStateCreateInfoNV" "coverageToColorLocation"
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
-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME"
pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = "VK_NV_fragment_coverage_to_color"
-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION"
pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION :: Integral a => a
pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV = VkStructureType 1000149000
