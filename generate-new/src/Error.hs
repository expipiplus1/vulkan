{-# LANGUAGE QuantifiedConstraints #-}

module Error
  ( Err
  , runErr
  , throw
  , context
  , contextShow
  , fromEither
  , note
  , several
  , traverseV
  , forV
  , HasErr
  , Sem
  )
where

import Relude
import Polysemy
import Data.Vector as V
import qualified Polysemy.Error as E
import Relude.Extra.Validation

type Err = E.Error (Vector Text)
type HasErr r = MemberWithError Err r

runErr :: forall r a. Sem (Err ': r) a -> Sem r (Either (Vector Text) a)
runErr = E.runError

throw :: forall r a. MemberWithError Err r => Text -> Sem r a
throw = E.throw . singleton

context :: forall r a . MemberWithError Err r => Text -> Sem r a -> Sem r a
context c m = E.catch @(Vector Text) m $ \e -> E.throw ((c <> ":" <+>) <$> e)

contextShow
  :: forall c r a . (Show c, MemberWithError Err r) => c -> Sem r a -> Sem r a
contextShow = context . show

fromEither :: MemberWithError Err r => Either Text a -> Sem r a
fromEither = E.fromEither . first singleton

note :: MemberWithError Err r => Text -> Maybe a -> Sem r a
note e = \case
  Nothing -> throw e
  Just x  -> pure x

several
  :: forall f r a . (Traversable f, HasErr r) => f (Sem r a) -> Sem r (f a)
several xs = do
  vs <- traverse
    (\x -> E.catch @(Vector Text) (Success <$> x) (pure . Failure))
    xs
  case sequenceA vs of
    Success as -> pure as
    Failure es -> E.throw es

traverseV
  :: forall f r a b
   . (Traversable f, HasErr r)
  => (a -> Sem r b)
  -> f a
  -> Sem r (f b)
traverseV f = several . fmap f

forV
  :: forall f r a b
   . (Traversable f, HasErr r)
  => f a
  -> (a -> Sem r b)
  -> Sem r (f b)
forV = flip traverseV

infixr 5 <+>
a <+> b = a <> " " <> b
