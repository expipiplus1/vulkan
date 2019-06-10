{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , VkSamplerReductionModeCreateInfoEXT(..)
  , VkSamplerReductionModeEXT(..)
  , pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
  , pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT
  , pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
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
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT"
data VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT = VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "filterMinmaxSingleComponentFormats"
  vkFilterMinmaxSingleComponentFormats :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "filterMinmaxImageComponentMapping"
  vkFilterMinmaxImageComponentMapping :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                              <*> peek (ptr `plusPtr` 8)
                                                              <*> peek (ptr `plusPtr` 16)
                                                              <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkFilterMinmaxSingleComponentFormats (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkFilterMinmaxImageComponentMapping (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))

instance Zero VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  zero = VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
                                                          zero
                                                          zero
                                                          zero

-- No documentation found for TopLevel "VkSamplerReductionModeCreateInfoEXT"
data VkSamplerReductionModeCreateInfoEXT = VkSamplerReductionModeCreateInfoEXT
  { -- No documentation found for Nested "VkSamplerReductionModeCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSamplerReductionModeCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSamplerReductionModeCreateInfoEXT" "reductionMode"
  vkReductionMode :: VkSamplerReductionModeEXT
  }
  deriving (Eq, Show)

instance Storable VkSamplerReductionModeCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerReductionModeCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerReductionModeCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerReductionModeCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkReductionMode (poked :: VkSamplerReductionModeCreateInfoEXT))

instance Zero VkSamplerReductionModeCreateInfoEXT where
  zero = VkSamplerReductionModeCreateInfoEXT VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
                                             zero
                                             zero

-- ** VkSamplerReductionModeEXT

-- No documentation found for TopLevel "VkSamplerReductionModeEXT"
newtype VkSamplerReductionModeEXT = VkSamplerReductionModeEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkSamplerReductionModeEXT where
  showsPrec _ VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = showString "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT"
  showsPrec _ VK_SAMPLER_REDUCTION_MODE_MIN_EXT = showString "VK_SAMPLER_REDUCTION_MODE_MIN_EXT"
  showsPrec _ VK_SAMPLER_REDUCTION_MODE_MAX_EXT = showString "VK_SAMPLER_REDUCTION_MODE_MAX_EXT"
  showsPrec p (VkSamplerReductionModeEXT x) = showParen (p >= 11) (showString "VkSamplerReductionModeEXT " . showsPrec 11 x)

instance Read VkSamplerReductionModeEXT where
  readPrec = parens ( choose [ ("VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT", pure VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT)
                             , ("VK_SAMPLER_REDUCTION_MODE_MIN_EXT",              pure VK_SAMPLER_REDUCTION_MODE_MIN_EXT)
                             , ("VK_SAMPLER_REDUCTION_MODE_MAX_EXT",              pure VK_SAMPLER_REDUCTION_MODE_MAX_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerReductionModeEXT")
                        v <- step readPrec
                        pure (VkSamplerReductionModeEXT v)
                        )
                    )

-- No documentation found for Nested "VkSamplerReductionModeEXT" "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT"
pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = VkSamplerReductionModeEXT 0

-- No documentation found for Nested "VkSamplerReductionModeEXT" "VK_SAMPLER_REDUCTION_MODE_MIN_EXT"
pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT = VkSamplerReductionModeEXT 1

-- No documentation found for Nested "VkSamplerReductionModeEXT" "VK_SAMPLER_REDUCTION_MODE_MAX_EXT"
pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT = VkSamplerReductionModeEXT 2

-- No documentation found for TopLevel "VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME"
pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME = "VK_EXT_sampler_filter_minmax"

-- No documentation found for TopLevel "VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION"
pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 1

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT = VkFormatFeatureFlagBits 0x00010000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT = VkStructureType 1000130000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT = VkStructureType 1000130001
