{-# language CPP #-}
module Graphics.Vulkan.Core10.OtherTypes  ( BufferMemoryBarrier
                                          , DispatchIndirectCommand
                                          , DrawIndexedIndirectCommand
                                          , DrawIndirectCommand
                                          , ImageMemoryBarrier
                                          , MemoryBarrier
                                          ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data BufferMemoryBarrier

instance ToCStruct BufferMemoryBarrier
instance Show BufferMemoryBarrier

instance FromCStruct BufferMemoryBarrier


data DispatchIndirectCommand

instance ToCStruct DispatchIndirectCommand
instance Show DispatchIndirectCommand

instance FromCStruct DispatchIndirectCommand


data DrawIndexedIndirectCommand

instance ToCStruct DrawIndexedIndirectCommand
instance Show DrawIndexedIndirectCommand

instance FromCStruct DrawIndexedIndirectCommand


data DrawIndirectCommand

instance ToCStruct DrawIndirectCommand
instance Show DrawIndirectCommand

instance FromCStruct DrawIndirectCommand


type role ImageMemoryBarrier nominal
data ImageMemoryBarrier (es :: [Type])

instance PokeChain es => ToCStruct (ImageMemoryBarrier es)
instance Show (Chain es) => Show (ImageMemoryBarrier es)

instance PeekChain es => FromCStruct (ImageMemoryBarrier es)


data MemoryBarrier

instance ToCStruct MemoryBarrier
instance Show MemoryBarrier

instance FromCStruct MemoryBarrier

