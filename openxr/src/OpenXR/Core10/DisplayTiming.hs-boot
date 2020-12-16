{-# language CPP #-}
-- No documentation found for Chapter "DisplayTiming"
module OpenXR.Core10.DisplayTiming  ( FrameBeginInfo
                                    , FrameEndInfo
                                    , FrameState
                                    , FrameWaitInfo
                                    , View
                                    , ViewLocateInfo
                                    , ViewState
                                    ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
data FrameBeginInfo

instance ToCStruct FrameBeginInfo
instance Show FrameBeginInfo

instance FromCStruct FrameBeginInfo


type role FrameEndInfo nominal
data FrameEndInfo (es :: [Type])

instance (Extendss FrameEndInfo es, PokeChain es) => ToCStruct (FrameEndInfo es)
instance Show (Chain es) => Show (FrameEndInfo es)

instance (Extendss FrameEndInfo es, PeekChain es) => FromCStruct (FrameEndInfo es)


type role FrameState nominal
data FrameState (es :: [Type])

instance (Extendss FrameState es, PokeChain es) => ToCStruct (FrameState es)
instance Show (Chain es) => Show (FrameState es)

instance (Extendss FrameState es, PeekChain es) => FromCStruct (FrameState es)


data FrameWaitInfo

instance ToCStruct FrameWaitInfo
instance Show FrameWaitInfo

instance FromCStruct FrameWaitInfo


data View

instance ToCStruct View
instance Show View

instance FromCStruct View


data ViewLocateInfo

instance ToCStruct ViewLocateInfo
instance Show ViewLocateInfo

instance FromCStruct ViewLocateInfo


data ViewState

instance ToCStruct ViewState
instance Show ViewState

instance FromCStruct ViewState

