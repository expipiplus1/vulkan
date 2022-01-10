{-# language DeriveAnyClass #-}

module Haskell.Name
  ( HName(..)
  , isTyConName
  )
where

import           Data.Text
import           Prettyprinter
import           Relude
import           Text.InterpolatedString.Perl6

import           Render.Utils                   ( unReservedWord )

data HName
  = TermName { unName :: Text }
  | TyConName { unName :: Text }
  | ConName  { unName :: Text }
  deriving(Eq, Ord, Show, Generic, Hashable)

isTyConName :: HName -> Bool
isTyConName = \case
  TyConName _ -> True
  _           -> False

instance Pretty HName where
  pretty = pretty . unReservedWord . unName

instance ShowQ HName where
  showQ = unpack . unReservedWord . unName

