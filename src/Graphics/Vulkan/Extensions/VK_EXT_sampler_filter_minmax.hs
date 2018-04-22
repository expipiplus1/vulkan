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
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  )


-- ** VkSamplerReductionModeEXT

-- | VkSamplerReductionModeEXT - Specify reduction mode for texture filtering
--
-- = See Also
--
-- 'VkSamplerReductionModeCreateInfoEXT'
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

-- | @VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT@ specifies that texel
-- values are combined by computing a weighted average of values in the
-- footprint, using weights as specified in [the image operations
-- chapter](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-unnormalized-to-integer).
pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = VkSamplerReductionModeEXT 0

-- | @VK_SAMPLER_REDUCTION_MODE_MIN_EXT@ specifies that texel values are
-- combined by taking the component-wise minimum of values in the footprint
-- with non-zero weights.
pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT = VkSamplerReductionModeEXT 1

-- | @VK_SAMPLER_REDUCTION_MODE_MAX_EXT@ specifies that texel values are
-- combined by taking the component-wise maximum of values in the footprint
-- with non-zero weights.
pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT :: VkSamplerReductionModeEXT
pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT = VkSamplerReductionModeEXT 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT = VkStructureType 1000130000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT = VkStructureType 1000130001
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT = VkFormatFeatureFlagBits 0x00010000
-- No documentation found for TopLevel "VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION"
pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME"
pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME = "VK_EXT_sampler_filter_minmax"
-- | VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT - Structure describing
-- sampler filter minmax limits that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @filterMinmaxSingleComponentFormats@ is a boolean value indicating
--     whether a minimum set of required formats support min\/max
--     filtering.
--
-- -   @filterMinmaxImageComponentMapping@ is a boolean value indicating
--     whether the implementation supports non-identity component mapping
--     of the image when doing min\/max filtering.
--
-- If the @VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- If @filterMinmaxSingleComponentFormats@ is @VK_TRUE@, the following
-- formats /must/ support the
-- @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT@ feature with
-- @VK_IMAGE_TILING_OPTIMAL@, if they support
-- @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@.
--
-- -   @VK_FORMAT_R8_UNORM@
--
-- -   @VK_FORMAT_R8_SNORM@
--
-- -   @VK_FORMAT_R16_UNORM@
--
-- -   @VK_FORMAT_R16_SNORM@
--
-- -   @VK_FORMAT_R16_SFLOAT@
--
-- -   @VK_FORMAT_R32_SFLOAT@
--
-- -   @VK_FORMAT_D16_UNORM@
--
-- -   @VK_FORMAT_X8_D24_UNORM_PACK32@
--
-- -   @VK_FORMAT_D32_SFLOAT@
--
-- -   @VK_FORMAT_D16_UNORM_S8_UINT@
--
-- -   @VK_FORMAT_D24_UNORM_S8_UINT@
--
-- -   @VK_FORMAT_D32_SFLOAT_S8_UINT@
--
-- If the format is a depth\/stencil format, this bit only specifies that
-- the depth aspect (not the stencil aspect) of an image of this format
-- supports min\/max filtering, and that min\/max filtering of the depth
-- aspect is supported when depth compare is disabled in the sampler.
--
-- If @filterMinmaxImageComponentMapping@ is @VK_FALSE@ the component
-- mapping of the image view used with min\/max filtering /must/ have been
-- created with the @r@ component set to @VK_COMPONENT_SWIZZLE_IDENTITY@.
-- Only the @r@ component of the sampled image value is defined and the
-- other component values are undefined. If
-- @filterMinmaxImageComponentMapping@ is @VK_TRUE@ this restriction does
-- not apply and image component mapping works as normal.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
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
-- | VkSamplerReductionModeCreateInfoEXT - Structure specifying sampler
-- reduction mode
--
-- = Description
--
-- If this structure is not present, @reductionMode@ is considered to be
-- @VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT@
--
-- -   @reductionMode@ /must/ be a valid 'VkSamplerReductionModeEXT' value
--
-- = See Also
--
-- 'VkSamplerReductionModeEXT',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkSamplerReductionModeCreateInfoEXT = VkSamplerReductionModeCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @reductionMode@ is an enum of type 'VkSamplerReductionModeEXT' that
  -- controls how texture filtering combines texel values.
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
