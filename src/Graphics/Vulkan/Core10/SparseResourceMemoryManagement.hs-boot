{-# language CPP #-}
module Graphics.Vulkan.Core10.SparseResourceMemoryManagement  ( BindSparseInfo
                                                              , SparseBufferMemoryBindInfo
                                                              , SparseImageFormatProperties
                                                              , SparseImageMemoryBind
                                                              , SparseImageMemoryBindInfo
                                                              , SparseImageMemoryRequirements
                                                              , SparseImageOpaqueMemoryBindInfo
                                                              , SparseMemoryBind
                                                              ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role BindSparseInfo nominal
data BindSparseInfo (es :: [Type])

instance PokeChain es => ToCStruct (BindSparseInfo es)
instance Show (Chain es) => Show (BindSparseInfo es)

instance PeekChain es => FromCStruct (BindSparseInfo es)


data SparseBufferMemoryBindInfo

instance ToCStruct SparseBufferMemoryBindInfo
instance Show SparseBufferMemoryBindInfo

instance FromCStruct SparseBufferMemoryBindInfo


data SparseImageFormatProperties

instance ToCStruct SparseImageFormatProperties
instance Show SparseImageFormatProperties

instance FromCStruct SparseImageFormatProperties


data SparseImageMemoryBind

instance ToCStruct SparseImageMemoryBind
instance Show SparseImageMemoryBind

instance FromCStruct SparseImageMemoryBind


data SparseImageMemoryBindInfo

instance ToCStruct SparseImageMemoryBindInfo
instance Show SparseImageMemoryBindInfo

instance FromCStruct SparseImageMemoryBindInfo


data SparseImageMemoryRequirements

instance ToCStruct SparseImageMemoryRequirements
instance Show SparseImageMemoryRequirements

instance FromCStruct SparseImageMemoryRequirements


data SparseImageOpaqueMemoryBindInfo

instance ToCStruct SparseImageOpaqueMemoryBindInfo
instance Show SparseImageOpaqueMemoryBindInfo

instance FromCStruct SparseImageOpaqueMemoryBindInfo


data SparseMemoryBind

instance ToCStruct SparseMemoryBind
instance Show SparseMemoryBind

instance FromCStruct SparseMemoryBind

