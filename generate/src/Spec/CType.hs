module Spec.CType where

data BuiltInType = Void
  deriving Show

data TypeQuals = TypeQuals { const :: Bool
                           , volatile :: Bool
                           }
  deriving Show

noTypeQuals = TypeQuals False False

data Type = TTypeDef String TypeQuals
          | TBuiltIn BuiltInType TypeQuals
          | TPtr Type TypeQuals
  deriving Show
