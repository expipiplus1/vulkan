{-# language DeriveAnyClass #-}

module Haskell.Name
  ( HName(..)
  )
where

import           Relude
import           Data.Text.Prettyprint.Doc
import           Text.InterpolatedString.Perl6
import           Data.Text

data HName
  = TermName { unName :: Text }
  | TyConName { unName :: Text }
  | ConName  { unName :: Text }
  deriving(Eq, Ord, Show, Generic, Hashable)

instance Pretty HName where
  pretty = pretty . unName

instance ShowQ HName where
  showQ = unpack . unName

