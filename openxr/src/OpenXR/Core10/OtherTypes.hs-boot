{-# language CPP #-}
-- No documentation found for Chapter "OtherTypes"
module OpenXR.Core10.OtherTypes  ( Color4f
                                 , CompositionLayerBaseHeader
                                 , SomeCompositionLayerBaseHeader
                                 , CompositionLayerProjection
                                 , CompositionLayerProjectionView
                                 , CompositionLayerQuad
                                 , EventDataBaseHeader
                                 , SomeEventDataBaseHeader
                                 , EventDataEventsLost
                                 , EventDataInstanceLossPending
                                 , EventDataInteractionProfileChanged
                                 , EventDataReferenceSpaceChangePending
                                 , EventDataSessionStateChanged
                                 , Fovf
                                 , HapticVibration
                                 , SwapchainSubImage
                                 , Vector4f
                                 ) where

import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct (FromCStruct)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct (ToCStruct)
data Color4f

instance ToCStruct Color4f
instance Show Color4f

instance FromCStruct Color4f


type role CompositionLayerBaseHeader nominal
data CompositionLayerBaseHeader (es :: [Type])

data SomeCompositionLayerBaseHeader

instance (Extendss CompositionLayerBaseHeader es, PokeChain es) => ToCStruct (CompositionLayerBaseHeader es)
instance Show (Chain es) => Show (CompositionLayerBaseHeader es)

instance (Extendss CompositionLayerBaseHeader es, PeekChain es) => FromCStruct (CompositionLayerBaseHeader es)


data CompositionLayerProjection

instance ToCStruct CompositionLayerProjection
instance Show CompositionLayerProjection

instance FromCStruct CompositionLayerProjection


type role CompositionLayerProjectionView nominal
data CompositionLayerProjectionView (es :: [Type])

instance (Extendss CompositionLayerProjectionView es, PokeChain es) => ToCStruct (CompositionLayerProjectionView es)
instance Show (Chain es) => Show (CompositionLayerProjectionView es)

instance (Extendss CompositionLayerProjectionView es, PeekChain es) => FromCStruct (CompositionLayerProjectionView es)


data CompositionLayerQuad

instance ToCStruct CompositionLayerQuad
instance Show CompositionLayerQuad

instance FromCStruct CompositionLayerQuad


data EventDataBaseHeader

data SomeEventDataBaseHeader

instance ToCStruct EventDataBaseHeader
instance Show EventDataBaseHeader

instance FromCStruct EventDataBaseHeader


data EventDataEventsLost

instance ToCStruct EventDataEventsLost
instance Show EventDataEventsLost

instance FromCStruct EventDataEventsLost


data EventDataInstanceLossPending

instance ToCStruct EventDataInstanceLossPending
instance Show EventDataInstanceLossPending

instance FromCStruct EventDataInstanceLossPending


data EventDataInteractionProfileChanged

instance ToCStruct EventDataInteractionProfileChanged
instance Show EventDataInteractionProfileChanged

instance FromCStruct EventDataInteractionProfileChanged


data EventDataReferenceSpaceChangePending

instance ToCStruct EventDataReferenceSpaceChangePending
instance Show EventDataReferenceSpaceChangePending

instance FromCStruct EventDataReferenceSpaceChangePending


data EventDataSessionStateChanged

instance ToCStruct EventDataSessionStateChanged
instance Show EventDataSessionStateChanged

instance FromCStruct EventDataSessionStateChanged


data Fovf

instance ToCStruct Fovf
instance Show Fovf

instance FromCStruct Fovf


data HapticVibration

instance ToCStruct HapticVibration
instance Show HapticVibration

instance FromCStruct HapticVibration


data SwapchainSubImage

instance ToCStruct SwapchainSubImage
instance Show SwapchainSubImage

instance FromCStruct SwapchainSubImage


data Vector4f

instance ToCStruct Vector4f
instance Show Vector4f

instance FromCStruct Vector4f

