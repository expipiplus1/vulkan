module Haskell.Name
  ( HName(..)
  )
where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Relude
import           Language.Haskell.TH.Syntax

data HName
  = TermName { unName :: Text }
  | TyConName { unName :: Text }
  | ConName  { unName :: Text }
  deriving(Eq, Ord, Show)

