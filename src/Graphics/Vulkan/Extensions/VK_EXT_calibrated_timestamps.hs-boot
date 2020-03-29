{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps  ( CalibratedTimestampInfoEXT
                                                                , TimeDomainEXT
                                                                ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data CalibratedTimestampInfoEXT

instance ToCStruct CalibratedTimestampInfoEXT
instance Show CalibratedTimestampInfoEXT

instance FromCStruct CalibratedTimestampInfoEXT


data TimeDomainEXT

