{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_bind_memory2"
module Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2  ( BindBufferMemoryInfo
                                                        , BindImageMemoryInfo
                                                        ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role BindBufferMemoryInfo nominal
data BindBufferMemoryInfo (es :: [Type])

instance (Extendss BindBufferMemoryInfo es, PokeChain es) => ToCStruct (BindBufferMemoryInfo es)
instance Show (Chain es) => Show (BindBufferMemoryInfo es)

instance (Extendss BindBufferMemoryInfo es, PeekChain es) => FromCStruct (BindBufferMemoryInfo es)


type role BindImageMemoryInfo nominal
data BindImageMemoryInfo (es :: [Type])

instance (Extendss BindImageMemoryInfo es, PokeChain es) => ToCStruct (BindImageMemoryInfo es)
instance Show (Chain es) => Show (BindImageMemoryInfo es)

instance (Extendss BindImageMemoryInfo es, PeekChain es) => FromCStruct (BindImageMemoryInfo es)

