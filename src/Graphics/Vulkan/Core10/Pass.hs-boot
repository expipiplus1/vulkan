{-# language CPP #-}
module Graphics.Vulkan.Core10.Pass  ( AttachmentDescription
                                    , AttachmentReference
                                    , FramebufferCreateInfo
                                    , RenderPassCreateInfo
                                    , SubpassDependency
                                    , SubpassDescription
                                    ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data AttachmentDescription

instance ToCStruct AttachmentDescription
instance Show AttachmentDescription

instance FromCStruct AttachmentDescription


data AttachmentReference

instance ToCStruct AttachmentReference
instance Show AttachmentReference

instance FromCStruct AttachmentReference


type role FramebufferCreateInfo nominal
data FramebufferCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (FramebufferCreateInfo es)
instance Show (Chain es) => Show (FramebufferCreateInfo es)

instance PeekChain es => FromCStruct (FramebufferCreateInfo es)


type role RenderPassCreateInfo nominal
data RenderPassCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (RenderPassCreateInfo es)
instance Show (Chain es) => Show (RenderPassCreateInfo es)

instance PeekChain es => FromCStruct (RenderPassCreateInfo es)


data SubpassDependency

instance ToCStruct SubpassDependency
instance Show SubpassDependency

instance FromCStruct SubpassDependency


data SubpassDescription

instance ToCStruct SubpassDescription
instance Show SubpassDescription

instance FromCStruct SubpassDescription

