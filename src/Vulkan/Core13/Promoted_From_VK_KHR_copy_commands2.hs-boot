{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_copy_commands2"
module Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2  ( BlitImageInfo2
                                                          , BufferCopy2
                                                          , BufferImageCopy2
                                                          , CopyBufferInfo2
                                                          , CopyBufferToImageInfo2
                                                          , CopyImageInfo2
                                                          , CopyImageToBufferInfo2
                                                          , ImageBlit2
                                                          , ImageCopy2
                                                          , ImageResolve2
                                                          , ResolveImageInfo2
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BlitImageInfo2

instance ToCStruct BlitImageInfo2
instance Show BlitImageInfo2

instance FromCStruct BlitImageInfo2


data BufferCopy2

instance ToCStruct BufferCopy2
instance Show BufferCopy2

instance FromCStruct BufferCopy2


type role BufferImageCopy2 nominal
data BufferImageCopy2 (es :: [Type])

instance (Extendss BufferImageCopy2 es, PokeChain es) => ToCStruct (BufferImageCopy2 es)
instance Show (Chain es) => Show (BufferImageCopy2 es)

instance (Extendss BufferImageCopy2 es, PeekChain es) => FromCStruct (BufferImageCopy2 es)


data CopyBufferInfo2

instance ToCStruct CopyBufferInfo2
instance Show CopyBufferInfo2

instance FromCStruct CopyBufferInfo2


data CopyBufferToImageInfo2

instance ToCStruct CopyBufferToImageInfo2
instance Show CopyBufferToImageInfo2

instance FromCStruct CopyBufferToImageInfo2


data CopyImageInfo2

instance ToCStruct CopyImageInfo2
instance Show CopyImageInfo2

instance FromCStruct CopyImageInfo2


data CopyImageToBufferInfo2

instance ToCStruct CopyImageToBufferInfo2
instance Show CopyImageToBufferInfo2

instance FromCStruct CopyImageToBufferInfo2


type role ImageBlit2 nominal
data ImageBlit2 (es :: [Type])

instance (Extendss ImageBlit2 es, PokeChain es) => ToCStruct (ImageBlit2 es)
instance Show (Chain es) => Show (ImageBlit2 es)

instance (Extendss ImageBlit2 es, PeekChain es) => FromCStruct (ImageBlit2 es)


data ImageCopy2

instance ToCStruct ImageCopy2
instance Show ImageCopy2

instance FromCStruct ImageCopy2


data ImageResolve2

instance ToCStruct ImageResolve2
instance Show ImageResolve2

instance FromCStruct ImageResolve2


data ResolveImageInfo2

instance ToCStruct ResolveImageInfo2
instance Show ResolveImageInfo2

instance FromCStruct ResolveImageInfo2

