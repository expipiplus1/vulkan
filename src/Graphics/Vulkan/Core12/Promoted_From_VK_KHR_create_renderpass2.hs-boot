{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2  ( AttachmentDescription2
                                                                       , AttachmentReference2
                                                                       , RenderPassCreateInfo2
                                                                       , SubpassBeginInfo
                                                                       , SubpassDependency2
                                                                       , SubpassDescription2
                                                                       , SubpassEndInfo
                                                                       ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role AttachmentDescription2 nominal
data AttachmentDescription2 (es :: [Type])

instance PokeChain es => ToCStruct (AttachmentDescription2 es)
instance Show (Chain es) => Show (AttachmentDescription2 es)

instance PeekChain es => FromCStruct (AttachmentDescription2 es)


type role AttachmentReference2 nominal
data AttachmentReference2 (es :: [Type])

instance PokeChain es => ToCStruct (AttachmentReference2 es)
instance Show (Chain es) => Show (AttachmentReference2 es)

instance PeekChain es => FromCStruct (AttachmentReference2 es)


type role RenderPassCreateInfo2 nominal
data RenderPassCreateInfo2 (es :: [Type])

instance PokeChain es => ToCStruct (RenderPassCreateInfo2 es)
instance Show (Chain es) => Show (RenderPassCreateInfo2 es)

instance PeekChain es => FromCStruct (RenderPassCreateInfo2 es)


data SubpassBeginInfo

instance ToCStruct SubpassBeginInfo
instance Show SubpassBeginInfo

instance FromCStruct SubpassBeginInfo


data SubpassDependency2

instance ToCStruct SubpassDependency2
instance Show SubpassDependency2

instance FromCStruct SubpassDependency2


type role SubpassDescription2 nominal
data SubpassDescription2 (es :: [Type])

instance PokeChain es => ToCStruct (SubpassDescription2 es)
instance Show (Chain es) => Show (SubpassDescription2 es)

instance PeekChain es => FromCStruct (SubpassDescription2 es)


data SubpassEndInfo

instance ToCStruct SubpassEndInfo
instance Show SubpassEndInfo

instance FromCStruct SubpassEndInfo

