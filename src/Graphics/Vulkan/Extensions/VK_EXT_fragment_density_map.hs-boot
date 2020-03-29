{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map  ( PhysicalDeviceFragmentDensityMapFeaturesEXT
                                                               , PhysicalDeviceFragmentDensityMapPropertiesEXT
                                                               , RenderPassFragmentDensityMapCreateInfoEXT
                                                               ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceFragmentDensityMapFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT
instance Show PhysicalDeviceFragmentDensityMapFeaturesEXT

instance FromCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT


data PhysicalDeviceFragmentDensityMapPropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT
instance Show PhysicalDeviceFragmentDensityMapPropertiesEXT

instance FromCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT


data RenderPassFragmentDensityMapCreateInfoEXT

instance ToCStruct RenderPassFragmentDensityMapCreateInfoEXT
instance Show RenderPassFragmentDensityMapCreateInfoEXT

instance FromCStruct RenderPassFragmentDensityMapCreateInfoEXT

