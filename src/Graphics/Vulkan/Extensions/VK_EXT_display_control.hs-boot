{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_display_control  ( DeviceEventInfoEXT
                                                          , DisplayEventInfoEXT
                                                          , DisplayPowerInfoEXT
                                                          , SwapchainCounterCreateInfoEXT
                                                          ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DeviceEventInfoEXT

instance ToCStruct DeviceEventInfoEXT
instance Show DeviceEventInfoEXT

instance FromCStruct DeviceEventInfoEXT


data DisplayEventInfoEXT

instance ToCStruct DisplayEventInfoEXT
instance Show DisplayEventInfoEXT

instance FromCStruct DisplayEventInfoEXT


data DisplayPowerInfoEXT

instance ToCStruct DisplayPowerInfoEXT
instance Show DisplayPowerInfoEXT

instance FromCStruct DisplayPowerInfoEXT


data SwapchainCounterCreateInfoEXT

instance ToCStruct SwapchainCounterCreateInfoEXT
instance Show SwapchainCounterCreateInfoEXT

instance FromCStruct SwapchainCounterCreateInfoEXT

