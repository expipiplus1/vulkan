module Render.Stmts.Poke.SiblingInfo
  where

import           Prettyprinter
import           Polysemy
import           Polysemy.Input
import           Relude

import           Error
import           Marshal.Scheme
import           Spec.Name

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
