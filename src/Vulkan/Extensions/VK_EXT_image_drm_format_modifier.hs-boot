{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_image_drm_format_modifier  ( DrmFormatModifierPropertiesEXT
                                                           , DrmFormatModifierPropertiesListEXT
                                                           , ImageDrmFormatModifierExplicitCreateInfoEXT
                                                           , ImageDrmFormatModifierListCreateInfoEXT
                                                           , ImageDrmFormatModifierPropertiesEXT
                                                           , PhysicalDeviceImageDrmFormatModifierInfoEXT
                                                           ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DrmFormatModifierPropertiesEXT

instance ToCStruct DrmFormatModifierPropertiesEXT
instance Show DrmFormatModifierPropertiesEXT

instance FromCStruct DrmFormatModifierPropertiesEXT


data DrmFormatModifierPropertiesListEXT

instance ToCStruct DrmFormatModifierPropertiesListEXT
instance Show DrmFormatModifierPropertiesListEXT

instance FromCStruct DrmFormatModifierPropertiesListEXT


data ImageDrmFormatModifierExplicitCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierExplicitCreateInfoEXT
instance Show ImageDrmFormatModifierExplicitCreateInfoEXT

instance FromCStruct ImageDrmFormatModifierExplicitCreateInfoEXT


data ImageDrmFormatModifierListCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierListCreateInfoEXT
instance Show ImageDrmFormatModifierListCreateInfoEXT

instance FromCStruct ImageDrmFormatModifierListCreateInfoEXT


data ImageDrmFormatModifierPropertiesEXT

instance ToCStruct ImageDrmFormatModifierPropertiesEXT
instance Show ImageDrmFormatModifierPropertiesEXT

instance FromCStruct ImageDrmFormatModifierPropertiesEXT


data PhysicalDeviceImageDrmFormatModifierInfoEXT

instance ToCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT
instance Show PhysicalDeviceImageDrmFormatModifierInfoEXT

instance FromCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT

