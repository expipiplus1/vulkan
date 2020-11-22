{-# LANGUAGE RoleAnnotations #-}
module Render.Stmts.Poke.SiblingInfo where

import           Data.Kind                      ( Type )
import           Polysemy
import           Polysemy.Input
import           Prelude
import           Spec.Name                      ( CName )

type role SiblingInfo nominal
data SiblingInfo (a :: Type)
type HasSiblingInfo a r = Member (Input (CName -> Maybe (SiblingInfo a))) r
