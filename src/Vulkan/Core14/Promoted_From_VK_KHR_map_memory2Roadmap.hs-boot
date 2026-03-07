{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_map_memory2Roadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap  ( MemoryMapInfo
                                                              , MemoryUnmapInfo
                                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role MemoryMapInfo nominal
data MemoryMapInfo (es :: [Type])

instance ( Extendss MemoryMapInfo es
         , PokeChain es ) => ToCStruct (MemoryMapInfo es)
instance Show (Chain es) => Show (MemoryMapInfo es)

instance ( Extendss MemoryMapInfo es
         , PeekChain es ) => FromCStruct (MemoryMapInfo es)


data MemoryUnmapInfo

instance ToCStruct MemoryUnmapInfo
instance Show MemoryUnmapInfo

instance FromCStruct MemoryUnmapInfo

