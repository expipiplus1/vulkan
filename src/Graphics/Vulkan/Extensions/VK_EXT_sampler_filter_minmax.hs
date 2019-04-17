{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  ( withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  , fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  , PhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , withCStructSamplerReductionModeCreateInfoEXT
  , fromCStructSamplerReductionModeCreateInfoEXT
  , SamplerReductionModeCreateInfoEXT(..)
  , SamplerReductionModeEXT
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


-- No documentation found for TopLevel "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT"
data PhysicalDeviceSamplerFilterMinmaxPropertiesEXT = PhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "filterMinmaxSingleComponentFormats"
  vkFilterMinmaxSingleComponentFormats :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "filterMinmaxImageComponentMapping"
  vkFilterMinmaxImageComponentMapping :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT -> (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT pPNext (boolToBool32 (vkFilterMinmaxSingleComponentFormats (from :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT))) (boolToBool32 (vkFilterMinmaxImageComponentMapping (from :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT)))))
fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT -> IO PhysicalDeviceSamplerFilterMinmaxPropertiesEXT
fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT c = PhysicalDeviceSamplerFilterMinmaxPropertiesEXT <$> -- Univalued Member elided
                                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT)))
                                                                                                             <*> pure (bool32ToBool (vkFilterMinmaxSingleComponentFormats (c :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT)))
                                                                                                             <*> pure (bool32ToBool (vkFilterMinmaxImageComponentMapping (c :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT)))
instance Zero PhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  zero = PhysicalDeviceSamplerFilterMinmaxPropertiesEXT Nothing
                                                        False
                                                        False
-- No documentation found for TopLevel "SamplerReductionModeCreateInfoEXT"
data SamplerReductionModeCreateInfoEXT = SamplerReductionModeCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "SamplerReductionModeCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerReductionModeCreateInfoEXT" "reductionMode"
  vkReductionMode :: SamplerReductionModeEXT
  }
  deriving (Show, Eq)
withCStructSamplerReductionModeCreateInfoEXT :: SamplerReductionModeCreateInfoEXT -> (VkSamplerReductionModeCreateInfoEXT -> IO a) -> IO a
withCStructSamplerReductionModeCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: SamplerReductionModeCreateInfoEXT)) (\pPNext -> cont (VkSamplerReductionModeCreateInfoEXT VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT pPNext (vkReductionMode (from :: SamplerReductionModeCreateInfoEXT))))
fromCStructSamplerReductionModeCreateInfoEXT :: VkSamplerReductionModeCreateInfoEXT -> IO SamplerReductionModeCreateInfoEXT
fromCStructSamplerReductionModeCreateInfoEXT c = SamplerReductionModeCreateInfoEXT <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerReductionModeCreateInfoEXT)))
                                                                                   <*> pure (vkReductionMode (c :: VkSamplerReductionModeCreateInfoEXT))
instance Zero SamplerReductionModeCreateInfoEXT where
  zero = SamplerReductionModeCreateInfoEXT Nothing
                                           zero
-- No documentation found for TopLevel "SamplerReductionModeEXT"
type SamplerReductionModeEXT = VkSamplerReductionModeEXT
