module TrackDepends
  where

import Relude
import Polysemy.Writer

newtype ModuleName = ModuleName { unModuleName :: Text }

data Dependency
  = External ModuleName Text
  | Internal Text

type TrackDepends = Writer [Dependency]
