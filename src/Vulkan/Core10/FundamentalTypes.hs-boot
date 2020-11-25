{-# language CPP #-}
-- No documentation found for Chapter "FundamentalTypes"
module Vulkan.Core10.FundamentalTypes  ( Extent2D
                                       , Extent3D
                                       , Offset2D
                                       , Offset3D
                                       , Rect2D
                                       , Bool32
                                       , DeviceAddress
                                       , DeviceSize
                                       ) where

import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data Extent2D

instance ToCStruct Extent2D
instance Show Extent2D

instance FromCStruct Extent2D


data Extent3D

instance ToCStruct Extent3D
instance Show Extent3D

instance FromCStruct Extent3D


data Offset2D

instance ToCStruct Offset2D
instance Show Offset2D

instance FromCStruct Offset2D


data Offset3D

instance ToCStruct Offset3D
instance Show Offset3D

instance FromCStruct Offset3D


data Rect2D

instance ToCStruct Rect2D
instance Show Rect2D

instance FromCStruct Rect2D


data Bool32


-- No documentation found for TopLevel "VkDeviceAddress"
type DeviceAddress = Word64


-- No documentation found for TopLevel "VkDeviceSize"
type DeviceSize = Word64

