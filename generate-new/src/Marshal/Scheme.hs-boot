{-# language RoleAnnotations #-}
module Marshal.Scheme
  where

import Data.Kind

type role MarshalScheme nominal
data MarshalScheme (a :: Type)
