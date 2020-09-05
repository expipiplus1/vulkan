{-# LANGUAGE QuantifiedConstraints #-}

module Error
  ( Err
  , runErr
  , throw
  , throwMany
  , context
  , contextShow
  , fromEither
  , fromEitherShow
  , note
  , sequenceV
  , traverseV
  , traverseV_
  , forV
  , forV_
  , HasErr
  , Sem
  )
where

import           Data.Vector                   as V
                                         hiding ( toList )
import           Polysemy
import qualified Polysemy.Error                as E
import           Relude
import           Validation

type Err = E.Error (Vector Text)
type HasErr r = MemberWithError Err r

runErr :: forall r a. Sem (Err ': r) a -> Sem r (Either (Vector Text) a)
runErr = E.runError

throw :: forall r a. MemberWithError Err r => Text -> Sem r a
throw = E.throw . singleton

throwMany
  :: forall r f a . (MemberWithError Err r, Foldable f) => f Text -> Sem r a
throwMany = E.throw . V.fromList . toList

context :: forall r a . MemberWithError Err r => Text -> Sem r a -> Sem r a
context c m = E.catch @(Vector Text) m $ \e -> E.throw ((c <> ":" <+>) <$> e)

contextShow
  :: forall c r a . (Show c, MemberWithError Err r) => c -> Sem r a -> Sem r a
contextShow = context . show

fromEither :: MemberWithError Err r => Either Text a -> Sem r a
fromEither = E.fromEither . first singleton

fromEitherShow :: (Show e, MemberWithError Err r) => Either e a -> Sem r a
fromEitherShow = fromEither . first show

note :: MemberWithError Err r => Text -> Maybe a -> Sem r a
note e = \case
  Nothing -> throw e
  Just x  -> pure x

sequenceV
  :: forall f r a . (Traversable f, HasErr r) => f (Sem r a) -> Sem r (f a)
sequenceV xs = do
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
traverseV f = sequenceV . fmap f

traverseV_
  :: forall f r a b
   . (Foldable f, HasErr r)
  => (a -> Sem r b)
  -> f a
  -> Sem r ()
traverseV_ f = void . traverseV f . toList

forV
  :: forall f r a b
   . (Traversable f, HasErr r)
  => f a
  -> (a -> Sem r b)
  -> Sem r (f b)
forV = flip traverseV

forV_
  :: forall f r a
   . (Foldable f, HasErr r)
  => f a
  -> (a -> Sem r ())
  -> Sem r ()
forV_ = flip traverseV_

infixr 5 <+>
(<+>) :: (Semigroup a, IsString a) => a -> a -> a
a <+> b = a <> " " <> b
