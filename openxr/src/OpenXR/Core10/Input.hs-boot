{-# language CPP #-}
-- No documentation found for Chapter "Input"
module OpenXR.Core10.Input  ( ActionCreateInfo
                            , ActionSetCreateInfo
                            , ActionStateBoolean
                            , ActionStateFloat
                            , ActionStateGetInfo
                            , ActionStatePose
                            , ActionStateVector2f
                            , ActionSuggestedBinding
                            , ActionsSyncInfo
                            , ActiveActionSet
                            , BoundSourcesForActionEnumerateInfo
                            , InputSourceLocalizedNameGetInfo
                            , InteractionProfileState
                            , InteractionProfileSuggestedBinding
                            , SessionActionSetsAttachInfo
                            , Vector2f
                            ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
data ActionCreateInfo

instance ToCStruct ActionCreateInfo
instance Show ActionCreateInfo

instance FromCStruct ActionCreateInfo


data ActionSetCreateInfo

instance ToCStruct ActionSetCreateInfo
instance Show ActionSetCreateInfo

instance FromCStruct ActionSetCreateInfo


data ActionStateBoolean

instance ToCStruct ActionStateBoolean
instance Show ActionStateBoolean

instance FromCStruct ActionStateBoolean


data ActionStateFloat

instance ToCStruct ActionStateFloat
instance Show ActionStateFloat

instance FromCStruct ActionStateFloat


data ActionStateGetInfo

instance ToCStruct ActionStateGetInfo
instance Show ActionStateGetInfo

instance FromCStruct ActionStateGetInfo


data ActionStatePose

instance ToCStruct ActionStatePose
instance Show ActionStatePose

instance FromCStruct ActionStatePose


data ActionStateVector2f

instance ToCStruct ActionStateVector2f
instance Show ActionStateVector2f

instance FromCStruct ActionStateVector2f


data ActionSuggestedBinding

instance ToCStruct ActionSuggestedBinding
instance Show ActionSuggestedBinding

instance FromCStruct ActionSuggestedBinding


data ActionsSyncInfo

instance ToCStruct ActionsSyncInfo
instance Show ActionsSyncInfo

instance FromCStruct ActionsSyncInfo


data ActiveActionSet

instance ToCStruct ActiveActionSet
instance Show ActiveActionSet

instance FromCStruct ActiveActionSet


data BoundSourcesForActionEnumerateInfo

instance ToCStruct BoundSourcesForActionEnumerateInfo
instance Show BoundSourcesForActionEnumerateInfo

instance FromCStruct BoundSourcesForActionEnumerateInfo


data InputSourceLocalizedNameGetInfo

instance ToCStruct InputSourceLocalizedNameGetInfo
instance Show InputSourceLocalizedNameGetInfo

instance FromCStruct InputSourceLocalizedNameGetInfo


data InteractionProfileState

instance ToCStruct InteractionProfileState
instance Show InteractionProfileState

instance FromCStruct InteractionProfileState


type role InteractionProfileSuggestedBinding nominal
data InteractionProfileSuggestedBinding (es :: [Type])

instance (Extendss InteractionProfileSuggestedBinding es, PokeChain es) => ToCStruct (InteractionProfileSuggestedBinding es)
instance Show (Chain es) => Show (InteractionProfileSuggestedBinding es)

instance (Extendss InteractionProfileSuggestedBinding es, PeekChain es) => FromCStruct (InteractionProfileSuggestedBinding es)


data SessionActionSetsAttachInfo

instance ToCStruct SessionActionSetsAttachInfo
instance Show SessionActionSetsAttachInfo

instance FromCStruct SessionActionSetsAttachInfo


data Vector2f

instance ToCStruct Vector2f
instance Show Vector2f

instance FromCStruct Vector2f

