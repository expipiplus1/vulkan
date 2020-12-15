{-# language CPP #-}
-- No documentation found for Chapter "CommandBuffer"
module Vulkan.Core10.CommandBuffer  ( CommandBufferAllocateInfo
                                    , CommandBufferBeginInfo
                                    , CommandBufferInheritanceInfo
                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data CommandBufferAllocateInfo

instance ToCStruct CommandBufferAllocateInfo
instance Show CommandBufferAllocateInfo

instance FromCStruct CommandBufferAllocateInfo


type role CommandBufferBeginInfo nominal
data CommandBufferBeginInfo (es :: [Type])

instance (Extendss CommandBufferBeginInfo es, PokeChain es) => ToCStruct (CommandBufferBeginInfo es)
instance Show (Chain es) => Show (CommandBufferBeginInfo es)

instance (Extendss CommandBufferBeginInfo es, PeekChain es) => FromCStruct (CommandBufferBeginInfo es)


type role CommandBufferInheritanceInfo nominal
data CommandBufferInheritanceInfo (es :: [Type])

instance (Extendss CommandBufferInheritanceInfo es, PokeChain es) => ToCStruct (CommandBufferInheritanceInfo es)
instance Show (Chain es) => Show (CommandBufferInheritanceInfo es)

instance (Extendss CommandBufferInheritanceInfo es, PeekChain es) => FromCStruct (CommandBufferInheritanceInfo es)

