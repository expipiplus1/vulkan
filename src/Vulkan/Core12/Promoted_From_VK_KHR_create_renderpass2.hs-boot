{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_create_renderpass2"
module Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2  ( AttachmentDescription2
                                                              , AttachmentReference2
                                                              , RenderPassCreateInfo2
                                                              , SubpassBeginInfo
                                                              , SubpassDependency2
                                                              , SubpassDescription2
                                                              , SubpassEndInfo
                                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role AttachmentDescription2 nominal
data AttachmentDescription2 (es :: [Type])

instance ( Extendss AttachmentDescription2 es
         , PokeChain es ) => ToCStruct (AttachmentDescription2 es)
instance Show (Chain es) => Show (AttachmentDescription2 es)

instance ( Extendss AttachmentDescription2 es
         , PeekChain es ) => FromCStruct (AttachmentDescription2 es)


type role AttachmentReference2 nominal
data AttachmentReference2 (es :: [Type])

instance ( Extendss AttachmentReference2 es
         , PokeChain es ) => ToCStruct (AttachmentReference2 es)
instance Show (Chain es) => Show (AttachmentReference2 es)

instance ( Extendss AttachmentReference2 es
         , PeekChain es ) => FromCStruct (AttachmentReference2 es)


type role RenderPassCreateInfo2 nominal
data RenderPassCreateInfo2 (es :: [Type])

instance ( Extendss RenderPassCreateInfo2 es
         , PokeChain es ) => ToCStruct (RenderPassCreateInfo2 es)
instance Show (Chain es) => Show (RenderPassCreateInfo2 es)

instance ( Extendss RenderPassCreateInfo2 es
         , PeekChain es ) => FromCStruct (RenderPassCreateInfo2 es)


data SubpassBeginInfo

instance ToCStruct SubpassBeginInfo
instance Show SubpassBeginInfo

instance FromCStruct SubpassBeginInfo


type role SubpassDependency2 nominal
data SubpassDependency2 (es :: [Type])

instance ( Extendss SubpassDependency2 es
         , PokeChain es ) => ToCStruct (SubpassDependency2 es)
instance Show (Chain es) => Show (SubpassDependency2 es)

instance ( Extendss SubpassDependency2 es
         , PeekChain es ) => FromCStruct (SubpassDependency2 es)


type role SubpassDescription2 nominal
data SubpassDescription2 (es :: [Type])

instance ( Extendss SubpassDescription2 es
         , PokeChain es ) => ToCStruct (SubpassDescription2 es)
instance Show (Chain es) => Show (SubpassDescription2 es)

instance ( Extendss SubpassDescription2 es
         , PeekChain es ) => FromCStruct (SubpassDescription2 es)


type role SubpassEndInfo nominal
data SubpassEndInfo (es :: [Type])

instance ( Extendss SubpassEndInfo es
         , PokeChain es ) => ToCStruct (SubpassEndInfo es)
instance Show (Chain es) => Show (SubpassEndInfo es)

instance ( Extendss SubpassEndInfo es
         , PeekChain es ) => FromCStruct (SubpassEndInfo es)

