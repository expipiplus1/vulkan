{-# language CPP #-}
module Graphics.Vulkan.Core10.CommandBuffer  ( CommandBufferAllocateInfo
                                             , CommandBufferBeginInfo
                                             , CommandBufferInheritanceInfo
                                             ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data CommandBufferAllocateInfo

instance ToCStruct CommandBufferAllocateInfo
instance Show CommandBufferAllocateInfo

instance FromCStruct CommandBufferAllocateInfo


type role CommandBufferBeginInfo nominal
data CommandBufferBeginInfo (es :: [Type])

instance PokeChain es => ToCStruct (CommandBufferBeginInfo es)
instance Show (Chain es) => Show (CommandBufferBeginInfo es)

instance PeekChain es => FromCStruct (CommandBufferBeginInfo es)


type role CommandBufferInheritanceInfo nominal
data CommandBufferInheritanceInfo (es :: [Type])

instance PokeChain es => ToCStruct (CommandBufferInheritanceInfo es)
instance Show (Chain es) => Show (CommandBufferInheritanceInfo es)

instance PeekChain es => FromCStruct (CommandBufferInheritanceInfo es)

