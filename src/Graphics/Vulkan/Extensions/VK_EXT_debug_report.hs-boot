{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_debug_report  ( DebugReportCallbackCreateInfoEXT
                                                       , DebugReportFlagBitsEXT
                                                       , DebugReportFlagsEXT
                                                       , DebugReportObjectTypeEXT
                                                       ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DebugReportCallbackCreateInfoEXT

instance ToCStruct DebugReportCallbackCreateInfoEXT
instance Show DebugReportCallbackCreateInfoEXT

instance FromCStruct DebugReportCallbackCreateInfoEXT


data DebugReportFlagBitsEXT

type DebugReportFlagsEXT = DebugReportFlagBitsEXT


data DebugReportObjectTypeEXT

