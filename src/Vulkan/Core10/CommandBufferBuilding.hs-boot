{-# language CPP #-}
-- No documentation found for Chapter "CommandBufferBuilding"
module Vulkan.Core10.CommandBufferBuilding  ( BufferCopy
                                            , BufferImageCopy
                                            , BufferMemoryBarrier
                                            , ClearAttachment
                                            , ClearDepthStencilValue
                                            , ClearRect
                                            , ImageBlit
                                            , ImageCopy
                                            , ImageMemoryBarrier
                                            , ImageResolve
                                            , ImageSubresourceLayers
                                            , MemoryBarrier
                                            , RenderPassBeginInfo
                                            , ClearColorValue
                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BufferCopy

instance ToCStruct BufferCopy
instance Show BufferCopy

instance FromCStruct BufferCopy


data BufferImageCopy

instance ToCStruct BufferImageCopy
instance Show BufferImageCopy

instance FromCStruct BufferImageCopy


type role BufferMemoryBarrier nominal
data BufferMemoryBarrier (es :: [Type])

instance ( Extendss BufferMemoryBarrier es
         , PokeChain es ) => ToCStruct (BufferMemoryBarrier es)
instance Show (Chain es) => Show (BufferMemoryBarrier es)

instance ( Extendss BufferMemoryBarrier es
         , PeekChain es ) => FromCStruct (BufferMemoryBarrier es)


data ClearAttachment

instance ToCStruct ClearAttachment
instance Show ClearAttachment


data ClearDepthStencilValue

instance ToCStruct ClearDepthStencilValue
instance Show ClearDepthStencilValue

instance FromCStruct ClearDepthStencilValue


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


type role ImageMemoryBarrier nominal
data ImageMemoryBarrier (es :: [Type])

instance ( Extendss ImageMemoryBarrier es
         , PokeChain es ) => ToCStruct (ImageMemoryBarrier es)
instance Show (Chain es) => Show (ImageMemoryBarrier es)

instance ( Extendss ImageMemoryBarrier es
         , PeekChain es ) => FromCStruct (ImageMemoryBarrier es)


data ImageResolve

instance ToCStruct ImageResolve
instance Show ImageResolve

instance FromCStruct ImageResolve


data ImageSubresourceLayers

instance ToCStruct ImageSubresourceLayers
instance Show ImageSubresourceLayers

instance FromCStruct ImageSubresourceLayers


data MemoryBarrier

instance ToCStruct MemoryBarrier
instance Show MemoryBarrier

instance FromCStruct MemoryBarrier


type role RenderPassBeginInfo nominal
data RenderPassBeginInfo (es :: [Type])

instance ( Extendss RenderPassBeginInfo es
         , PokeChain es ) => ToCStruct (RenderPassBeginInfo es)
instance Show (Chain es) => Show (RenderPassBeginInfo es)


data ClearColorValue

