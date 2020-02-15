module Marshall
  where

import Relude
import Polysemy
import Polysemy.Writer

import CType
import Haskell
import TrackDepends

data MarshalledFun = MarshalledFun
  { marshalledCType :: Sem '[TrackDepends] HType
  , marshalledHType :: Sem '[TrackDepends] HType
  , marshalledToC :: Sem '[TrackDepends] Text
  }

data Parameter = Parameter
  { paramName :: Text
  , paramType :: CType
  , paramLength :: Maybe ParameterLength
  , paramIsOptional :: Maybe Bool
  }
  deriving (Show, Eq)

data ParameterLength
  = NullTerminated
  | NamedLength Text
  | NamedMemberLength Text Text
    -- ^ The length is specified by a member of another (struct) parameter
  deriving (Show, Eq)

marshallFunction :: CType -> Parameter -> MarshalledFun
marshallFunction ret args = undefined

qualifiedCon
  :: Member TrackDepends r
  => Text -> Text -> Sem r HType
qualifiedCon mod' t = do
  tell [External (ModuleName mod') t]
  pure (Con t)
