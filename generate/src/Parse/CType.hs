module Parse.CType where

import Spec.CType

parseCType :: String -> Type
parseCType s = TTypeDef ("FIXME " ++ s ++ " FIXME") noTypeQuals
