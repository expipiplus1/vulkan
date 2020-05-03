{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_debug_report  ( DebugReportCallbackCreateInfoEXT
                                              , DebugReportFlagBitsEXT
                                              , DebugReportFlagsEXT
                                              , DebugReportObjectTypeEXT
                                              ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DebugReportCallbackCreateInfoEXT

instance ToCStruct DebugReportCallbackCreateInfoEXT
instance Show DebugReportCallbackCreateInfoEXT

instance FromCStruct DebugReportCallbackCreateInfoEXT


data DebugReportFlagBitsEXT

type DebugReportFlagsEXT = DebugReportFlagBitsEXT


data DebugReportObjectTypeEXT

