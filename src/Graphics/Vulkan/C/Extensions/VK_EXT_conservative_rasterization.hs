{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( VkConservativeRasterizationModeEXT(..)
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
  , VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , VkPipelineRasterizationConservativeStateCreateFlagsEXT(..)
  , VkPipelineRasterizationConservativeStateCreateInfoEXT(..)
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
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


-- ** VkConservativeRasterizationModeEXT

-- No documentation found for TopLevel "VkConservativeRasterizationModeEXT"
newtype VkConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkConservativeRasterizationModeEXT where
  showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT"
  showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT"
  showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT"
  showsPrec p (VkConservativeRasterizationModeEXT x) = showParen (p >= 11) (showString "VkConservativeRasterizationModeEXT " . showsPrec 11 x)

instance Read VkConservativeRasterizationModeEXT where
  readPrec = parens ( choose [ ("VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT",      pure VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT)
                             , ("VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT",  pure VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT)
                             , ("VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT", pure VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkConservativeRasterizationModeEXT")
                        v <- step readPrec
                        pure (VkConservativeRasterizationModeEXT v)
                        )
                    )

-- No documentation found for Nested "VkConservativeRasterizationModeEXT" "VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT"
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = VkConservativeRasterizationModeEXT 0

-- No documentation found for Nested "VkConservativeRasterizationModeEXT" "VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT"
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = VkConservativeRasterizationModeEXT 1

-- No documentation found for Nested "VkConservativeRasterizationModeEXT" "VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT"
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = VkConservativeRasterizationModeEXT 2

-- No documentation found for TopLevel "VkPhysicalDeviceConservativeRasterizationPropertiesEXT"
data VkPhysicalDeviceConservativeRasterizationPropertiesEXT = VkPhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveOverestimationSize"
  vkPrimitiveOverestimationSize :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "maxExtraPrimitiveOverestimationSize"
  vkMaxExtraPrimitiveOverestimationSize :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "extraPrimitiveOverestimationSizeGranularity"
  vkExtraPrimitiveOverestimationSizeGranularity :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveUnderestimation"
  vkPrimitiveUnderestimation :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativePointAndLineRasterization"
  vkConservativePointAndLineRasterization :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateTrianglesRasterized"
  vkDegenerateTrianglesRasterized :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateLinesRasterized"
  vkDegenerateLinesRasterized :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "fullyCoveredFragmentShaderInputVariable"
  vkFullyCoveredFragmentShaderInputVariable :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativeRasterizationPostDepthCoverage"
  vkConservativeRasterizationPostDepthCoverage :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceConservativeRasterizationPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                                    <*> peek (ptr `plusPtr` 8)
                                                                    <*> peek (ptr `plusPtr` 16)
                                                                    <*> peek (ptr `plusPtr` 20)
                                                                    <*> peek (ptr `plusPtr` 24)
                                                                    <*> peek (ptr `plusPtr` 28)
                                                                    <*> peek (ptr `plusPtr` 32)
                                                                    <*> peek (ptr `plusPtr` 36)
                                                                    <*> peek (ptr `plusPtr` 40)
                                                                    <*> peek (ptr `plusPtr` 44)
                                                                    <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkPrimitiveOverestimationSize (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxExtraPrimitiveOverestimationSize (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkExtraPrimitiveOverestimationSizeGranularity (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkPrimitiveUnderestimation (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkConservativePointAndLineRasterization (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkDegenerateTrianglesRasterized (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkDegenerateLinesRasterized (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 44) (vkFullyCoveredFragmentShaderInputVariable (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 48) (vkConservativeRasterizationPostDepthCoverage (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))

instance Zero VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  zero = VkPhysicalDeviceConservativeRasterizationPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero

-- ** VkPipelineRasterizationConservativeStateCreateFlagsEXT

-- No documentation found for TopLevel "VkPipelineRasterizationConservativeStateCreateFlagsEXT"
newtype VkPipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkPipelineRasterizationConservativeStateCreateFlagsEXT where
  
  showsPrec p (VkPipelineRasterizationConservativeStateCreateFlagsEXT x) = showParen (p >= 11) (showString "VkPipelineRasterizationConservativeStateCreateFlagsEXT " . showsPrec 11 x)

instance Read VkPipelineRasterizationConservativeStateCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineRasterizationConservativeStateCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkPipelineRasterizationConservativeStateCreateFlagsEXT v)
                        )
                    )



-- No documentation found for TopLevel "VkPipelineRasterizationConservativeStateCreateInfoEXT"
data VkPipelineRasterizationConservativeStateCreateInfoEXT = VkPipelineRasterizationConservativeStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineRasterizationConservativeStateCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineRasterizationConservativeStateCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineRasterizationConservativeStateCreateInfoEXT" "flags"
  vkFlags :: VkPipelineRasterizationConservativeStateCreateFlagsEXT
  , -- No documentation found for Nested "VkPipelineRasterizationConservativeStateCreateInfoEXT" "conservativeRasterizationMode"
  vkConservativeRasterizationMode :: VkConservativeRasterizationModeEXT
  , -- No documentation found for Nested "VkPipelineRasterizationConservativeStateCreateInfoEXT" "extraPrimitiveOverestimationSize"
  vkExtraPrimitiveOverestimationSize :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkPipelineRasterizationConservativeStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineRasterizationConservativeStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
                                                                   <*> peek (ptr `plusPtr` 20)
                                                                   <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkConservativeRasterizationMode (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkExtraPrimitiveOverestimationSize (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))

instance Zero VkPipelineRasterizationConservativeStateCreateInfoEXT where
  zero = VkPipelineRasterizationConservativeStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
                                                               zero
                                                               zero
                                                               zero
                                                               zero

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME"
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION"
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: Integral a => a
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT = VkStructureType 1000101000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT = VkStructureType 1000101001
