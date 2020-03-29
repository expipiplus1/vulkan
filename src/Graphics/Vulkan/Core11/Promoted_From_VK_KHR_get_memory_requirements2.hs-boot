{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2  ( BufferMemoryRequirementsInfo2
                                                                             , ImageMemoryRequirementsInfo2
                                                                             , ImageSparseMemoryRequirementsInfo2
                                                                             , MemoryRequirements2
                                                                             , SparseImageMemoryRequirements2
                                                                             , MemoryRequirements2KHR
                                                                             ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data BufferMemoryRequirementsInfo2

instance ToCStruct BufferMemoryRequirementsInfo2
instance Show BufferMemoryRequirementsInfo2

instance FromCStruct BufferMemoryRequirementsInfo2


type role ImageMemoryRequirementsInfo2 nominal
data ImageMemoryRequirementsInfo2 (es :: [Type])

instance PokeChain es => ToCStruct (ImageMemoryRequirementsInfo2 es)
instance Show (Chain es) => Show (ImageMemoryRequirementsInfo2 es)

instance PeekChain es => FromCStruct (ImageMemoryRequirementsInfo2 es)


data ImageSparseMemoryRequirementsInfo2

instance ToCStruct ImageSparseMemoryRequirementsInfo2
instance Show ImageSparseMemoryRequirementsInfo2

instance FromCStruct ImageSparseMemoryRequirementsInfo2


type role MemoryRequirements2 nominal
data MemoryRequirements2 (es :: [Type])

instance PokeChain es => ToCStruct (MemoryRequirements2 es)
instance Show (Chain es) => Show (MemoryRequirements2 es)

instance PeekChain es => FromCStruct (MemoryRequirements2 es)


data SparseImageMemoryRequirements2

instance ToCStruct SparseImageMemoryRequirements2
instance Show SparseImageMemoryRequirements2

instance FromCStruct SparseImageMemoryRequirements2


-- No documentation found for TopLevel "VkMemoryRequirements2KHR"
type MemoryRequirements2KHR = MemoryRequirements2

