module Render.Stmts.Poke.SiblingInfo
  where

import           Relude
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Input

import           Error
import           Spec.Name
import {-# SOURCE #-} Marshal.Scheme

data SiblingInfo a = SiblingInfo
  { siReferrer :: Doc ()
    -- ^ How to refer to this sibling in code
  , siScheme :: MarshalScheme a
    -- ^ What type is this sibling
  }

type HasSiblingInfo a r = Member (Input (CName -> Maybe (SiblingInfo a))) r

getSiblingInfo
  :: forall a r
   . (HasErr r, HasSiblingInfo a r)
  => CName
  -> Sem r (SiblingInfo a)
getSiblingInfo n =
  note ("Unable to find info for: " <> unCName n) . ($ n) =<< input
