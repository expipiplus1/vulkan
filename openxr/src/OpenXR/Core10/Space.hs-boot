{-# language CPP #-}
-- No documentation found for Chapter "Space"
module OpenXR.Core10.Space  ( ActionSpaceCreateInfo
                            , Posef
                            , Quaternionf
                            , ReferenceSpaceCreateInfo
                            , SpaceLocation
                            , SpaceVelocity
                            , Vector3f
                            ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
data ActionSpaceCreateInfo

instance ToCStruct ActionSpaceCreateInfo
instance Show ActionSpaceCreateInfo

instance FromCStruct ActionSpaceCreateInfo


data Posef

instance ToCStruct Posef
instance Show Posef

instance FromCStruct Posef


data Quaternionf

instance ToCStruct Quaternionf
instance Show Quaternionf

instance FromCStruct Quaternionf


data ReferenceSpaceCreateInfo

instance ToCStruct ReferenceSpaceCreateInfo
instance Show ReferenceSpaceCreateInfo

instance FromCStruct ReferenceSpaceCreateInfo


type role SpaceLocation nominal
data SpaceLocation (es :: [Type])

instance (Extendss SpaceLocation es, PokeChain es) => ToCStruct (SpaceLocation es)
instance Show (Chain es) => Show (SpaceLocation es)

instance (Extendss SpaceLocation es, PeekChain es) => FromCStruct (SpaceLocation es)


data SpaceVelocity

instance ToCStruct SpaceVelocity
instance Show SpaceVelocity

instance FromCStruct SpaceVelocity


data Vector3f

instance ToCStruct Vector3f
instance Show Vector3f

instance FromCStruct Vector3f

