module Marshal.Name
  where

import Relude

data NameDomain
  = Unmarshaled
  | Marshaled

newtype Name (d :: NameDomain) = Name { unName :: Text }
