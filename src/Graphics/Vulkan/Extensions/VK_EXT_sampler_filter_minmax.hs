{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  ( withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  , fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  , PhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , withCStructSamplerReductionModeCreateInfoEXT
  , fromCStructSamplerReductionModeCreateInfoEXT
  , SamplerReductionModeCreateInfoEXT(..)
  , SamplerReductionModeEXT
  , pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
  , pattern SAMPLER_REDUCTION_MODE_MIN_EXT
  , pattern SAMPLER_REDUCTION_MODE_MAX_EXT
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , VkSamplerReductionModeCreateInfoEXT(..)
  , VkSamplerReductionModeEXT(..)
  , pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT
  , pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT
  , pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  )



-- | VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT - Structure describing
-- sampler filter minmax limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- If @filterMinmaxSingleComponentFormats@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the following formats /must/
-- support the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT'
-- feature with
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL',
-- if they support
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'.
--
-- If the format is a depth\/stencil format, this bit only specifies that
-- the depth aspect (not the stencil aspect) of an image of this format
-- supports min\/max filtering, and that min\/max filtering of the depth
-- aspect is supported when depth compare is disabled in the sampler.
--
-- If @filterMinmaxImageComponentMapping@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' the component mapping of the
-- image view used with min\/max filtering /must/ have been created with
-- the @r@ component set to
-- 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'. Only
-- the @r@ component of the sampled image value is defined and the other
-- component values are undefined. If @filterMinmaxImageComponentMapping@
-- is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' this restriction does not
-- apply and image component mapping works as normal.
--
-- Unresolved directive in
-- VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceSamplerFilterMinmaxPropertiesEXT = PhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "filterMinmaxSingleComponentFormats"
  filterMinmaxSingleComponentFormats :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "filterMinmaxImageComponentMapping"
  filterMinmaxImageComponentMapping :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT' and
-- marshal a 'PhysicalDeviceSamplerFilterMinmaxPropertiesEXT' into it. The 'VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT -> (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT pPNext (boolToBool32 (filterMinmaxSingleComponentFormats (marshalled :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT))) (boolToBool32 (filterMinmaxImageComponentMapping (marshalled :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT)))))

-- | A function to read a 'VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceSamplerFilterMinmaxPropertiesEXT'.
fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT -> IO PhysicalDeviceSamplerFilterMinmaxPropertiesEXT
fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT c = PhysicalDeviceSamplerFilterMinmaxPropertiesEXT <$> -- Univalued Member elided
                                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT)))
                                                                                                             <*> pure (bool32ToBool (vkFilterMinmaxSingleComponentFormats (c :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT)))
                                                                                                             <*> pure (bool32ToBool (vkFilterMinmaxImageComponentMapping (c :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT)))

instance Zero PhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  zero = PhysicalDeviceSamplerFilterMinmaxPropertiesEXT Nothing
                                                        False
                                                        False



-- | VkSamplerReductionModeCreateInfoEXT - Structure specifying sampler
-- reduction mode
--
-- = Description
--
-- If this structure is not present, @reductionMode@ is considered to be
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT'.
--
-- Unresolved directive in VkSamplerReductionModeCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkSamplerReductionModeCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data SamplerReductionModeCreateInfoEXT = SamplerReductionModeCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "SamplerReductionModeCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerReductionModeCreateInfoEXT" "reductionMode"
  reductionMode :: SamplerReductionModeEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSamplerReductionModeCreateInfoEXT' and
-- marshal a 'SamplerReductionModeCreateInfoEXT' into it. The 'VkSamplerReductionModeCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSamplerReductionModeCreateInfoEXT :: SamplerReductionModeCreateInfoEXT -> (VkSamplerReductionModeCreateInfoEXT -> IO a) -> IO a
withCStructSamplerReductionModeCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SamplerReductionModeCreateInfoEXT)) (\pPNext -> cont (VkSamplerReductionModeCreateInfoEXT VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT pPNext (reductionMode (marshalled :: SamplerReductionModeCreateInfoEXT))))

-- | A function to read a 'VkSamplerReductionModeCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'SamplerReductionModeCreateInfoEXT'.
fromCStructSamplerReductionModeCreateInfoEXT :: VkSamplerReductionModeCreateInfoEXT -> IO SamplerReductionModeCreateInfoEXT
fromCStructSamplerReductionModeCreateInfoEXT c = SamplerReductionModeCreateInfoEXT <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerReductionModeCreateInfoEXT)))
                                                                                   <*> pure (vkReductionMode (c :: VkSamplerReductionModeCreateInfoEXT))

instance Zero SamplerReductionModeCreateInfoEXT where
  zero = SamplerReductionModeCreateInfoEXT Nothing
                                           zero


-- | VkSamplerReductionModeEXT - Specify reduction mode for texture filtering
--
-- = See Also
--
-- No cross-references are available
type SamplerReductionModeEXT = VkSamplerReductionModeEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT'
-- specifies that texel values are combined by computing a weighted average
-- of values in the footprint, using weights as specified in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-unnormalized-to-integer the image operations chapter>.
pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT :: (a ~ SamplerReductionModeEXT) => a
pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VK_SAMPLER_REDUCTION_MODE_MIN_EXT'
-- specifies that texel values are combined by taking the component-wise
-- minimum of values in the footprint with non-zero weights.
pattern SAMPLER_REDUCTION_MODE_MIN_EXT :: (a ~ SamplerReductionModeEXT) => a
pattern SAMPLER_REDUCTION_MODE_MIN_EXT = VK_SAMPLER_REDUCTION_MODE_MIN_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VK_SAMPLER_REDUCTION_MODE_MAX_EXT'
-- specifies that texel values are combined by taking the component-wise
-- maximum of values in the footprint with non-zero weights.
pattern SAMPLER_REDUCTION_MODE_MAX_EXT :: (a ~ SamplerReductionModeEXT) => a
pattern SAMPLER_REDUCTION_MODE_MAX_EXT = VK_SAMPLER_REDUCTION_MODE_MAX_EXT
