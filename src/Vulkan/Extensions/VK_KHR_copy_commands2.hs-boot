{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_copy_commands2"
module Vulkan.Extensions.VK_KHR_copy_commands2  ( BlitImageInfo2KHR
                                                , BufferCopy2KHR
                                                , BufferImageCopy2KHR
                                                , CopyBufferInfo2KHR
                                                , CopyBufferToImageInfo2KHR
                                                , CopyImageInfo2KHR
                                                , CopyImageToBufferInfo2KHR
                                                , ImageBlit2KHR
                                                , ImageCopy2KHR
                                                , ImageResolve2KHR
                                                , ResolveImageInfo2KHR
                                                ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
data BlitImageInfo2KHR

instance ToCStruct BlitImageInfo2KHR
instance Show BlitImageInfo2KHR

instance FromCStruct BlitImageInfo2KHR


data BufferCopy2KHR

instance ToCStruct BufferCopy2KHR
instance Show BufferCopy2KHR

instance FromCStruct BufferCopy2KHR


type role BufferImageCopy2KHR nominal
data BufferImageCopy2KHR (es :: [Type])

instance (Extendss BufferImageCopy2KHR es, PokeChain es) => ToCStruct (BufferImageCopy2KHR es)
instance Show (Chain es) => Show (BufferImageCopy2KHR es)

instance (Extendss BufferImageCopy2KHR es, PeekChain es) => FromCStruct (BufferImageCopy2KHR es)


data CopyBufferInfo2KHR

instance ToCStruct CopyBufferInfo2KHR
instance Show CopyBufferInfo2KHR

instance FromCStruct CopyBufferInfo2KHR


data CopyBufferToImageInfo2KHR

instance ToCStruct CopyBufferToImageInfo2KHR
instance Show CopyBufferToImageInfo2KHR

instance FromCStruct CopyBufferToImageInfo2KHR


data CopyImageInfo2KHR

instance ToCStruct CopyImageInfo2KHR
instance Show CopyImageInfo2KHR

instance FromCStruct CopyImageInfo2KHR


data CopyImageToBufferInfo2KHR

instance ToCStruct CopyImageToBufferInfo2KHR
instance Show CopyImageToBufferInfo2KHR

instance FromCStruct CopyImageToBufferInfo2KHR


type role ImageBlit2KHR nominal
data ImageBlit2KHR (es :: [Type])

instance (Extendss ImageBlit2KHR es, PokeChain es) => ToCStruct (ImageBlit2KHR es)
instance Show (Chain es) => Show (ImageBlit2KHR es)

instance (Extendss ImageBlit2KHR es, PeekChain es) => FromCStruct (ImageBlit2KHR es)


data ImageCopy2KHR

instance ToCStruct ImageCopy2KHR
instance Show ImageCopy2KHR

instance FromCStruct ImageCopy2KHR


data ImageResolve2KHR

instance ToCStruct ImageResolve2KHR
instance Show ImageResolve2KHR

instance FromCStruct ImageResolve2KHR


data ResolveImageInfo2KHR

instance ToCStruct ResolveImageInfo2KHR
instance Show ResolveImageInfo2KHR

instance FromCStruct ResolveImageInfo2KHR

