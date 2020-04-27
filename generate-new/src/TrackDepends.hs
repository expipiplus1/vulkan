module TrackDepends
  where

import           Polysemy
import           Polysemy.Writer
import           Relude

newtype ModuleName = ModuleName { unModuleName :: Text }

data Dependency
  = External ModuleName Text
  | Internal Text

type TrackDepends = Writer [Dependency]

type HasTrackDepends r = MemberWithError TrackDepends r
