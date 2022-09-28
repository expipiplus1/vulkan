{-# language CPP #-}
-- No documentation found for Chapter "Image"
module OpenXR.Core10.Image  ( SwapchainCreateInfo
                            , SwapchainImageAcquireInfo
                            , SwapchainImageBaseHeader
                            , SwapchainImageReleaseInfo
                            , SwapchainImageWaitInfo
                            ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
type role SwapchainCreateInfo nominal
data SwapchainCreateInfo (es :: [Type])

instance ( Extendss SwapchainCreateInfo es
         , PokeChain es ) => ToCStruct (SwapchainCreateInfo es)
instance Show (Chain es) => Show (SwapchainCreateInfo es)

instance ( Extendss SwapchainCreateInfo es
         , PeekChain es ) => FromCStruct (SwapchainCreateInfo es)


data SwapchainImageAcquireInfo

instance ToCStruct SwapchainImageAcquireInfo
instance Show SwapchainImageAcquireInfo

instance FromCStruct SwapchainImageAcquireInfo


data SwapchainImageBaseHeader

instance ToCStruct SwapchainImageBaseHeader
instance Show SwapchainImageBaseHeader

instance FromCStruct SwapchainImageBaseHeader


data SwapchainImageReleaseInfo

instance ToCStruct SwapchainImageReleaseInfo
instance Show SwapchainImageReleaseInfo

instance FromCStruct SwapchainImageReleaseInfo


data SwapchainImageWaitInfo

instance ToCStruct SwapchainImageWaitInfo
instance Show SwapchainImageWaitInfo

instance FromCStruct SwapchainImageWaitInfo

