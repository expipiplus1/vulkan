{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing  ( PastPresentationTimingGOOGLE
                                                            , PresentTimeGOOGLE
                                                            , PresentTimesInfoGOOGLE
                                                            , RefreshCycleDurationGOOGLE
                                                            ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PastPresentationTimingGOOGLE

instance ToCStruct PastPresentationTimingGOOGLE
instance Show PastPresentationTimingGOOGLE

instance FromCStruct PastPresentationTimingGOOGLE


data PresentTimeGOOGLE

instance ToCStruct PresentTimeGOOGLE
instance Show PresentTimeGOOGLE

instance FromCStruct PresentTimeGOOGLE


data PresentTimesInfoGOOGLE

instance ToCStruct PresentTimesInfoGOOGLE
instance Show PresentTimesInfoGOOGLE

instance FromCStruct PresentTimesInfoGOOGLE


data RefreshCycleDurationGOOGLE

instance ToCStruct RefreshCycleDurationGOOGLE
instance Show RefreshCycleDurationGOOGLE

instance FromCStruct RefreshCycleDurationGOOGLE

