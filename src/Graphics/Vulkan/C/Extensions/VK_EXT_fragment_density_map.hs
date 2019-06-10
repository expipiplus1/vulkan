{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( VkPhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , VkPhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , VkRenderPassFragmentDensityMapCreateInfoEXT(..)
  , pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
  , pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  , pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  , pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  , pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  ) where

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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  , VkImageUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageViewCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkAttachmentReference(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkSamplerCreateFlagBits(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMapFeaturesEXT"
data VkPhysicalDeviceFragmentDensityMapFeaturesEXT = VkPhysicalDeviceFragmentDensityMapFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMap"
  vkFragmentDensityMap :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapDynamic"
  vkFragmentDensityMapDynamic :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapNonSubsampledImages"
  vkFragmentDensityMapNonSubsampledImages :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFragmentDensityMapFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFragmentDensityMapFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 20)
                                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkFragmentDensityMap (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkFragmentDensityMapDynamic (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
                *> poke (ptr `plusPtr` 24) (vkFragmentDensityMapNonSubsampledImages (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))

instance Zero VkPhysicalDeviceFragmentDensityMapFeaturesEXT where
  zero = VkPhysicalDeviceFragmentDensityMapFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
                                                       zero
                                                       zero
                                                       zero
                                                       zero

-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMapPropertiesEXT"
data VkPhysicalDeviceFragmentDensityMapPropertiesEXT = VkPhysicalDeviceFragmentDensityMapPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "minFragmentDensityTexelSize"
  vkMinFragmentDensityTexelSize :: VkExtent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "maxFragmentDensityTexelSize"
  vkMaxFragmentDensityTexelSize :: VkExtent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "fragmentDensityInvocations"
  vkFragmentDensityInvocations :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFragmentDensityMapPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFragmentDensityMapPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 24)
                                                             <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMinFragmentDensityTexelSize (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkMaxFragmentDensityTexelSize (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkFragmentDensityInvocations (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))

instance Zero VkPhysicalDeviceFragmentDensityMapPropertiesEXT where
  zero = VkPhysicalDeviceFragmentDensityMapPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
                                                         zero
                                                         zero
                                                         zero
                                                         zero

-- No documentation found for TopLevel "VkRenderPassFragmentDensityMapCreateInfoEXT"
data VkRenderPassFragmentDensityMapCreateInfoEXT = VkRenderPassFragmentDensityMapCreateInfoEXT
  { -- No documentation found for Nested "VkRenderPassFragmentDensityMapCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassFragmentDensityMapCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassFragmentDensityMapCreateInfoEXT" "fragmentDensityMapAttachment"
  vkFragmentDensityMapAttachment :: VkAttachmentReference
  }
  deriving (Eq, Show)

instance Storable VkRenderPassFragmentDensityMapCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkRenderPassFragmentDensityMapCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassFragmentDensityMapCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassFragmentDensityMapCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFragmentDensityMapAttachment (poked :: VkRenderPassFragmentDensityMapCreateInfoEXT))

instance Zero VkRenderPassFragmentDensityMapCreateInfoEXT where
  zero = VkRenderPassFragmentDensityMapCreateInfoEXT VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
                                                     zero
                                                     zero

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT"
pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT = VkAccessFlagBits 0x01000000

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME"
pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = "VK_EXT_fragment_density_map"

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION"
pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION :: Integral a => a
pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT = VkFormatFeatureFlagBits 0x01000000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT = VkImageCreateFlagBits 0x00004000

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT"
pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT :: VkImageLayout
pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT = VkImageLayout 1000218000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT = VkImageUsageFlagBits 0x00000200

-- No documentation found for Nested "VkImageViewCreateFlagBits" "VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT"
pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT :: VkImageViewCreateFlagBits
pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT = VkImageViewCreateFlagBits 0x00000001

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT = VkPipelineStageFlagBits 0x00800000

-- No documentation found for Nested "VkSamplerCreateFlagBits" "VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT"
pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT :: VkSamplerCreateFlagBits
pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT = VkSamplerCreateFlagBits 0x00000001

-- No documentation found for Nested "VkSamplerCreateFlagBits" "VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT"
pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT :: VkSamplerCreateFlagBits
pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT = VkSamplerCreateFlagBits 0x00000002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT = VkStructureType 1000218000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT = VkStructureType 1000218001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT = VkStructureType 1000218002
