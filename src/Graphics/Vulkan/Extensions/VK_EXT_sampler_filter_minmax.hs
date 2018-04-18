{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  ( VkSamplerReductionModeEXT(..)
  , pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
  , pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT
  , pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  , VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , VkSamplerReductionModeCreateInfoEXT(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
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
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  )


-- ** VkSamplerReductionModeEXT

-- | 
newtype VkSamplerReductionModeEXT = VkSamplerReductionModeEXT Int32
  deriving (Eq, Ord, Storable)

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

-- | 
pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = VkSamplerReductionModeEXT 0

-- | 
pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT = VkSamplerReductionModeEXT 1

-- | 
pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT = VkSamplerReductionModeEXT 2
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT = VkStructureType 1000130000
-- | Nothing
pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT = VkStructureType 1000130001
-- | Just "Format can be used with min/max reduction filtering"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT = VkFormatFeatureFlagBits 0x00010000
pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 1
pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME = "VK_EXT_sampler_filter_minmax"
-- | TODO: Struct comments
data VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT = VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFilterMinmaxSingleComponentFormats :: VkBool32
  , vkFilterMinmaxImageComponentMapping :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkFilterMinmaxSingleComponentFormats (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkFilterMinmaxImageComponentMapping (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
-- | TODO: Struct comments
data VkSamplerReductionModeCreateInfoEXT = VkSamplerReductionModeCreateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkReductionMode :: VkSamplerReductionModeEXT
  }
  deriving (Eq, Show)

instance Storable VkSamplerReductionModeCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerReductionModeCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerReductionModeCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSamplerReductionModeCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkReductionMode (poked :: VkSamplerReductionModeCreateInfoEXT))
