{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2  ( BindBufferMemoryInfo
                                                                 , BindImageMemoryInfo
                                                                 ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role BindBufferMemoryInfo nominal
data BindBufferMemoryInfo (es :: [Type])

instance PokeChain es => ToCStruct (BindBufferMemoryInfo es)
instance Show (Chain es) => Show (BindBufferMemoryInfo es)

instance PeekChain es => FromCStruct (BindBufferMemoryInfo es)


type role BindImageMemoryInfo nominal
data BindImageMemoryInfo (es :: [Type])

instance PokeChain es => ToCStruct (BindImageMemoryInfo es)
instance Show (Chain es) => Show (BindImageMemoryInfo es)

instance PeekChain es => FromCStruct (BindImageMemoryInfo es)

