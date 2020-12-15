{-# language CPP #-}
-- No documentation found for Chapter "Memory"
module Vulkan.Core10.Memory  ( MappedMemoryRange
                             , MemoryAllocateInfo
                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data MappedMemoryRange

instance ToCStruct MappedMemoryRange
instance Show MappedMemoryRange

instance FromCStruct MappedMemoryRange


type role MemoryAllocateInfo nominal
data MemoryAllocateInfo (es :: [Type])

instance (Extendss MemoryAllocateInfo es, PokeChain es) => ToCStruct (MemoryAllocateInfo es)
instance Show (Chain es) => Show (MemoryAllocateInfo es)

instance (Extendss MemoryAllocateInfo es, PeekChain es) => FromCStruct (MemoryAllocateInfo es)

