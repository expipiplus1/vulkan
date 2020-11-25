{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_debug_report"
module Vulkan.Extensions.VK_EXT_debug_report  ( DebugReportCallbackCreateInfoEXT
                                              , DebugReportFlagsEXT
                                              , DebugReportFlagBitsEXT
                                              , DebugReportObjectTypeEXT
                                              ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DebugReportCallbackCreateInfoEXT

instance ToCStruct DebugReportCallbackCreateInfoEXT
instance Show DebugReportCallbackCreateInfoEXT

instance FromCStruct DebugReportCallbackCreateInfoEXT


type DebugReportFlagsEXT = DebugReportFlagBitsEXT

data DebugReportFlagBitsEXT


data DebugReportObjectTypeEXT

