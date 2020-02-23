module Haskell.Name
  ( HName(..)
  )
where

import           Data.Text                      ( Text )
import           Relude

data HName
  = TermName { unName :: Text }
  | TyConName { unName :: Text }
  | ConName  { unName :: Text }
  deriving(Eq, Ord, Show)

