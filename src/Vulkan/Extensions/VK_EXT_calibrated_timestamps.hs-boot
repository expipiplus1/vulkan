{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_calibrated_timestamps"
module Vulkan.Extensions.VK_EXT_calibrated_timestamps  ( CalibratedTimestampInfoEXT
                                                       , TimeDomainEXT
                                                       ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data CalibratedTimestampInfoEXT

instance ToCStruct CalibratedTimestampInfoEXT
instance Show CalibratedTimestampInfoEXT

instance FromCStruct CalibratedTimestampInfoEXT


data TimeDomainEXT

