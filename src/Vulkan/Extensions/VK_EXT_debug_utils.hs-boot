{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_debug_utils  ( DebugUtilsLabelEXT
                                             , DebugUtilsMessengerCallbackDataEXT
                                             , DebugUtilsMessengerCreateInfoEXT
                                             , DebugUtilsObjectNameInfoEXT
                                             , DebugUtilsObjectTagInfoEXT
                                             , DebugUtilsMessageSeverityFlagBitsEXT
                                             , DebugUtilsMessageSeverityFlagsEXT
                                             , DebugUtilsMessageTypeFlagBitsEXT
                                             , DebugUtilsMessageTypeFlagsEXT
                                             ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DebugUtilsLabelEXT

instance ToCStruct DebugUtilsLabelEXT
instance Show DebugUtilsLabelEXT

instance FromCStruct DebugUtilsLabelEXT


data DebugUtilsMessengerCallbackDataEXT

instance ToCStruct DebugUtilsMessengerCallbackDataEXT
instance Show DebugUtilsMessengerCallbackDataEXT

instance FromCStruct DebugUtilsMessengerCallbackDataEXT


data DebugUtilsMessengerCreateInfoEXT

instance ToCStruct DebugUtilsMessengerCreateInfoEXT
instance Show DebugUtilsMessengerCreateInfoEXT

instance FromCStruct DebugUtilsMessengerCreateInfoEXT


data DebugUtilsObjectNameInfoEXT

instance ToCStruct DebugUtilsObjectNameInfoEXT
instance Show DebugUtilsObjectNameInfoEXT

instance FromCStruct DebugUtilsObjectNameInfoEXT


data DebugUtilsObjectTagInfoEXT

instance ToCStruct DebugUtilsObjectTagInfoEXT
instance Show DebugUtilsObjectTagInfoEXT

instance FromCStruct DebugUtilsObjectTagInfoEXT


data DebugUtilsMessageSeverityFlagBitsEXT

type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT


data DebugUtilsMessageTypeFlagBitsEXT

type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

