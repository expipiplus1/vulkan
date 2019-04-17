{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map
  ( withCStructPhysicalDeviceFragmentDensityMapFeaturesEXT
  , fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT
  , PhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , withCStructPhysicalDeviceFragmentDensityMapPropertiesEXT
  , fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT
  , PhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , withCStructRenderPassFragmentDensityMapCreateInfoEXT
  , fromCStructRenderPassFragmentDensityMapCreateInfoEXT
  , RenderPassFragmentDensityMapCreateInfoEXT(..)
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  , pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  , pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  , pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  , pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  , pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( VkPhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , VkPhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , VkRenderPassFragmentDensityMapCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.Pass
  ( AttachmentReference(..)
  , fromCStructAttachmentReference
  , withCStructAttachmentReference
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , fromCStructExtent2D
  , withCStructExtent2D
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
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
  )


-- No documentation found for TopLevel "PhysicalDeviceFragmentDensityMapFeaturesEXT"
data PhysicalDeviceFragmentDensityMapFeaturesEXT = PhysicalDeviceFragmentDensityMapFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMap"
  vkFragmentDensityMap :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapDynamic"
  vkFragmentDensityMapDynamic :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapNonSubsampledImages"
  vkFragmentDensityMapNonSubsampledImages :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceFragmentDensityMapFeaturesEXT :: PhysicalDeviceFragmentDensityMapFeaturesEXT -> (VkPhysicalDeviceFragmentDensityMapFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceFragmentDensityMapFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceFragmentDensityMapFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceFragmentDensityMapFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT pPNext (boolToBool32 (vkFragmentDensityMap (from :: PhysicalDeviceFragmentDensityMapFeaturesEXT))) (boolToBool32 (vkFragmentDensityMapDynamic (from :: PhysicalDeviceFragmentDensityMapFeaturesEXT))) (boolToBool32 (vkFragmentDensityMapNonSubsampledImages (from :: PhysicalDeviceFragmentDensityMapFeaturesEXT)))))
fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT -> IO PhysicalDeviceFragmentDensityMapFeaturesEXT
fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT c = PhysicalDeviceFragmentDensityMapFeaturesEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkFragmentDensityMap (c :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkFragmentDensityMapDynamic (c :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkFragmentDensityMapNonSubsampledImages (c :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT)))
instance Zero PhysicalDeviceFragmentDensityMapFeaturesEXT where
  zero = PhysicalDeviceFragmentDensityMapFeaturesEXT Nothing
                                                     False
                                                     False
                                                     False
-- No documentation found for TopLevel "PhysicalDeviceFragmentDensityMapPropertiesEXT"
data PhysicalDeviceFragmentDensityMapPropertiesEXT = PhysicalDeviceFragmentDensityMapPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "minFragmentDensityTexelSize"
  vkMinFragmentDensityTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "maxFragmentDensityTexelSize"
  vkMaxFragmentDensityTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "fragmentDensityInvocations"
  vkFragmentDensityInvocations :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceFragmentDensityMapPropertiesEXT :: PhysicalDeviceFragmentDensityMapPropertiesEXT -> (VkPhysicalDeviceFragmentDensityMapPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceFragmentDensityMapPropertiesEXT from cont = withCStructExtent2D (vkMaxFragmentDensityTexelSize (from :: PhysicalDeviceFragmentDensityMapPropertiesEXT)) (\maxFragmentDensityTexelSize -> withCStructExtent2D (vkMinFragmentDensityTexelSize (from :: PhysicalDeviceFragmentDensityMapPropertiesEXT)) (\minFragmentDensityTexelSize -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceFragmentDensityMapPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceFragmentDensityMapPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT pPNext minFragmentDensityTexelSize maxFragmentDensityTexelSize (boolToBool32 (vkFragmentDensityInvocations (from :: PhysicalDeviceFragmentDensityMapPropertiesEXT)))))))
fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT -> IO PhysicalDeviceFragmentDensityMapPropertiesEXT
fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT c = PhysicalDeviceFragmentDensityMapPropertiesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT)))
                                                                                                           <*> (fromCStructExtent2D (vkMinFragmentDensityTexelSize (c :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT)))
                                                                                                           <*> (fromCStructExtent2D (vkMaxFragmentDensityTexelSize (c :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkFragmentDensityInvocations (c :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT)))
instance Zero PhysicalDeviceFragmentDensityMapPropertiesEXT where
  zero = PhysicalDeviceFragmentDensityMapPropertiesEXT Nothing
                                                       zero
                                                       zero
                                                       False
-- No documentation found for TopLevel "RenderPassFragmentDensityMapCreateInfoEXT"
data RenderPassFragmentDensityMapCreateInfoEXT = RenderPassFragmentDensityMapCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "RenderPassFragmentDensityMapCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassFragmentDensityMapCreateInfoEXT" "fragmentDensityMapAttachment"
  vkFragmentDensityMapAttachment :: AttachmentReference
  }
  deriving (Show, Eq)
withCStructRenderPassFragmentDensityMapCreateInfoEXT :: RenderPassFragmentDensityMapCreateInfoEXT -> (VkRenderPassFragmentDensityMapCreateInfoEXT -> IO a) -> IO a
withCStructRenderPassFragmentDensityMapCreateInfoEXT from cont = withCStructAttachmentReference (vkFragmentDensityMapAttachment (from :: RenderPassFragmentDensityMapCreateInfoEXT)) (\fragmentDensityMapAttachment -> maybeWith withSomeVkStruct (vkPNext (from :: RenderPassFragmentDensityMapCreateInfoEXT)) (\pPNext -> cont (VkRenderPassFragmentDensityMapCreateInfoEXT VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT pPNext fragmentDensityMapAttachment)))
fromCStructRenderPassFragmentDensityMapCreateInfoEXT :: VkRenderPassFragmentDensityMapCreateInfoEXT -> IO RenderPassFragmentDensityMapCreateInfoEXT
fromCStructRenderPassFragmentDensityMapCreateInfoEXT c = RenderPassFragmentDensityMapCreateInfoEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassFragmentDensityMapCreateInfoEXT)))
                                                                                                   <*> (fromCStructAttachmentReference (vkFragmentDensityMapAttachment (c :: VkRenderPassFragmentDensityMapCreateInfoEXT)))
instance Zero RenderPassFragmentDensityMapCreateInfoEXT where
  zero = RenderPassFragmentDensityMapCreateInfoEXT Nothing
                                                   zero
