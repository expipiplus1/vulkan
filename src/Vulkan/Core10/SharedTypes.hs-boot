{-# language CPP #-}
module Vulkan.Core10.SharedTypes  ( ClearDepthStencilValue
                                  , Extent2D
                                  , Extent3D
                                  , ImageSubresourceLayers
                                  , ImageSubresourceRange
                                  , Offset2D
                                  , Offset3D
                                  , ClearColorValue
                                  ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ClearDepthStencilValue

instance ToCStruct ClearDepthStencilValue
instance Show ClearDepthStencilValue

instance FromCStruct ClearDepthStencilValue


data Extent2D

instance ToCStruct Extent2D
instance Show Extent2D

instance FromCStruct Extent2D


data Extent3D

instance ToCStruct Extent3D
instance Show Extent3D

instance FromCStruct Extent3D


data ImageSubresourceLayers

instance ToCStruct ImageSubresourceLayers
instance Show ImageSubresourceLayers

instance FromCStruct ImageSubresourceLayers


data ImageSubresourceRange

instance ToCStruct ImageSubresourceRange
instance Show ImageSubresourceRange

instance FromCStruct ImageSubresourceRange


data Offset2D

instance ToCStruct Offset2D
instance Show Offset2D

instance FromCStruct Offset2D


data Offset3D

instance ToCStruct Offset3D
instance Show Offset3D

instance FromCStruct Offset3D


data ClearColorValue

