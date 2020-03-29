{-# language CPP #-}
module Graphics.Vulkan.Core10.CommandBufferBuilding  ( BufferCopy
                                                     , BufferImageCopy
                                                     , ClearAttachment
                                                     , ClearRect
                                                     , ImageBlit
                                                     , ImageCopy
                                                     , ImageResolve
                                                     , Rect2D
                                                     , RenderPassBeginInfo
                                                     , Viewport
                                                     ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data BufferCopy

instance ToCStruct BufferCopy
instance Show BufferCopy

instance FromCStruct BufferCopy


data BufferImageCopy

instance ToCStruct BufferImageCopy
instance Show BufferImageCopy

instance FromCStruct BufferImageCopy


data ClearAttachment

instance ToCStruct ClearAttachment
instance Show ClearAttachment


data ClearRect

instance ToCStruct ClearRect
instance Show ClearRect

instance FromCStruct ClearRect


data ImageBlit

instance ToCStruct ImageBlit
instance Show ImageBlit

instance FromCStruct ImageBlit


data ImageCopy

instance ToCStruct ImageCopy
instance Show ImageCopy

instance FromCStruct ImageCopy


data ImageResolve

instance ToCStruct ImageResolve
instance Show ImageResolve

instance FromCStruct ImageResolve


data Rect2D

instance ToCStruct Rect2D
instance Show Rect2D

instance FromCStruct Rect2D


type role RenderPassBeginInfo nominal
data RenderPassBeginInfo (es :: [Type])

instance PokeChain es => ToCStruct (RenderPassBeginInfo es)
instance Show (Chain es) => Show (RenderPassBeginInfo es)


data Viewport

instance ToCStruct Viewport
instance Show Viewport

instance FromCStruct Viewport

