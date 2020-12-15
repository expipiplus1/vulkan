{-# language CPP #-}
-- No documentation found for Chapter "Pass"
module Vulkan.Core10.Pass  ( AttachmentDescription
                           , AttachmentReference
                           , FramebufferCreateInfo
                           , RenderPassCreateInfo
                           , SubpassDependency
                           , SubpassDescription
                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
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

instance (Extendss FramebufferCreateInfo es, PokeChain es) => ToCStruct (FramebufferCreateInfo es)
instance Show (Chain es) => Show (FramebufferCreateInfo es)

instance (Extendss FramebufferCreateInfo es, PeekChain es) => FromCStruct (FramebufferCreateInfo es)


type role RenderPassCreateInfo nominal
data RenderPassCreateInfo (es :: [Type])

instance (Extendss RenderPassCreateInfo es, PokeChain es) => ToCStruct (RenderPassCreateInfo es)
instance Show (Chain es) => Show (RenderPassCreateInfo es)

instance (Extendss RenderPassCreateInfo es, PeekChain es) => FromCStruct (RenderPassCreateInfo es)


data SubpassDependency

instance ToCStruct SubpassDependency
instance Show SubpassDependency

instance FromCStruct SubpassDependency


data SubpassDescription

instance ToCStruct SubpassDescription
instance Show SubpassDescription

instance FromCStruct SubpassDescription

