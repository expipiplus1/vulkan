{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  ( VkConservativeRasterizationModeEXT(..)
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
  , VkPipelineRasterizationConservativeStateCreateFlagsEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , VkPipelineRasterizationConservativeStateCreateInfoEXT(..)
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


-- ** VkConservativeRasterizationModeEXT

-- | 
newtype VkConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT Int32
  deriving (Eq, Ord, Storable)

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

-- | 
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = VkConservativeRasterizationModeEXT 0

-- | 
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = VkConservativeRasterizationModeEXT 1

-- | 
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = VkConservativeRasterizationModeEXT 2
-- ** VkPipelineRasterizationConservativeStateCreateFlagsEXT

-- | 
newtype VkPipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT = VkStructureType 1000101000
-- | Nothing
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT = VkStructureType 1000101001
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: Integral a => a
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"
-- | TODO: Struct comments
data VkPhysicalDeviceConservativeRasterizationPropertiesEXT = VkPhysicalDeviceConservativeRasterizationPropertiesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkPrimitiveOverestimationSize :: CFloat
  , vkMaxExtraPrimitiveOverestimationSize :: CFloat
  , vkExtraPrimitiveOverestimationSizeGranularity :: CFloat
  , vkPrimitiveUnderestimation :: VkBool32
  , vkConservativePointAndLineRasterization :: VkBool32
  , vkDegenerateTrianglesRasterized :: VkBool32
  , vkDegenerateLinesRasterized :: VkBool32
  , vkFullyCoveredFragmentShaderInputVariable :: VkBool32
  , vkConservativeRasterizationPostDepthCoverage :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkPrimitiveOverestimationSize (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxExtraPrimitiveOverestimationSize (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkExtraPrimitiveOverestimationSizeGranularity (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkPrimitiveUnderestimation (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkConservativePointAndLineRasterization (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkDegenerateTrianglesRasterized (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkDegenerateLinesRasterized (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 44) (vkFullyCoveredFragmentShaderInputVariable (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 48) (vkConservativeRasterizationPostDepthCoverage (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
-- | TODO: Struct comments
data VkPipelineRasterizationConservativeStateCreateInfoEXT = VkPipelineRasterizationConservativeStateCreateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkPipelineRasterizationConservativeStateCreateFlagsEXT
  , vkConservativeRasterizationMode :: VkConservativeRasterizationModeEXT
  , vkExtraPrimitiveOverestimationSize :: CFloat
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkConservativeRasterizationMode (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkExtraPrimitiveOverestimationSize (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
