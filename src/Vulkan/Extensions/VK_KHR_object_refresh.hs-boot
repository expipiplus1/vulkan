{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_object_refresh"
module Vulkan.Extensions.VK_KHR_object_refresh  ( RefreshObjectKHR
                                                , RefreshObjectListKHR
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data RefreshObjectKHR

instance ToCStruct RefreshObjectKHR
instance Show RefreshObjectKHR

instance FromCStruct RefreshObjectKHR


data RefreshObjectListKHR

instance ToCStruct RefreshObjectListKHR
instance Show RefreshObjectListKHR

instance FromCStruct RefreshObjectListKHR

