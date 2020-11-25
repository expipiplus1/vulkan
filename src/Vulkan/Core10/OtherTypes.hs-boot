{-# language CPP #-}
-- No documentation found for Chapter "OtherTypes"
module Vulkan.Core10.OtherTypes  ( BufferMemoryBarrier
                                 , DispatchIndirectCommand
                                 , DrawIndexedIndirectCommand
                                 , DrawIndirectCommand
                                 , ImageMemoryBarrier
                                 , MemoryBarrier
                                 ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
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

instance (Extendss ImageMemoryBarrier es, PokeChain es) => ToCStruct (ImageMemoryBarrier es)
instance Show (Chain es) => Show (ImageMemoryBarrier es)

instance (Extendss ImageMemoryBarrier es, PeekChain es) => FromCStruct (ImageMemoryBarrier es)


data MemoryBarrier

instance ToCStruct MemoryBarrier
instance Show MemoryBarrier

instance FromCStruct MemoryBarrier

