{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A module for the monad much of the writing takes place in, it reports
-- errors and accumulates members for 'WriteElement's
module Write.Monad
  ( Write(..)
  , Lookup(..)
  , throw
  , forV
  , traverseV
  , WE(..)
  , liftWrite
  , runWE
  , runWE'
  , toHsType
  , toHsTypePrec
  , makeSourceDepends
  , tellBootElem
  , tellImport
  , tellQualifiedImport
  , tellImports
  , tellExport
  , tellUndependableExport
  , tellDepend
  , tellGuardedDepend
  , tellDepends
  , tellSourceDepend
  , tellExtension
  , toE
  , toV
  , toELookup
  , toVLookup
  ) where

import           Prelude                           hiding ( Enum )
import           Data.Text.Extra                   hiding ( head
                                                          , partition
                                                          , reverse
                                                          )
import           Data.List.Extra                          ( elem
                                                          , partition
                                                          )
import           Control.Monad.Except
import           Data.Either.Validation
import           Data.Traversable
import           Data.Monoid                              ( Ap(..) )
import           Data.Foldable
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Text.Prettyprint.Doc                ( Doc )
import           Data.Bifunctor

import           Write.Element
import           Write.Monad.Lookup
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Savvy.Type.Haskell       as H

newtype Write a = Write { unWrite :: ReaderT Lookup (Either [Text]) a }
  deriving (Functor, Applicative, Monad, MonadFix)

deriving instance MonadReader Lookup Write

instance MonadError Text Write where
  throwError = Write . throwError . pure
  catchError (Write a) h =
    Write $ catchError a (unWrite . h . head)
    -- case a of
    -- Left  []      -> error "empty errors"
    -- Left  (e : _) -> h e
    -- Right r       -> Write (Right r)

withContext :: Text -> Write a -> Write a
withContext context (Write e) =
  Write (mapReaderT (first (fmap ((context <> ":") <+>))) e)

throw :: Text -> Write a
throw message = Write $ throwError [message]

-- | flip traverseV
forV :: forall t a b . Traversable t => t a -> (a -> Write b) -> Write (t b)
forV t f = do
  Write $ do
    mapReaderT validationToEither
      $ for t (mapReaderT eitherToValidation . unWrite . f)

-- | Accumulate several errors while traversing a structure
traverseV :: Traversable t => (a -> Write b) -> t a -> Write (t b)
traverseV = flip forV

----------------------------------------------------------------
-- WE stuff
----------------------------------------------------------------

-- | A monad for generating write elements
newtype WE a = WE { unWE :: StateT WriteElement Write a }
  deriving (Functor, Applicative, Monad)
  deriving (Semigroup, Monoid) via (Ap WE a)

deriving instance MonadError Text WE

deriving instance MonadReader Lookup WE

liftWrite :: Write a -> WE a
liftWrite = WE . lift

runWE
  :: Text
  -- ^ Write element name
  -> WE (DocMap -> Doc ())
  -> Write WriteElement
runWE name = fmap fst . runWE' name . fmap (,())

runWE' :: Text -> WE (DocMap -> Doc (), a) -> Write (WriteElement, a)
runWE' name (WE we) = do
  ((d, x), e) <- withContext name $ runStateT we (emptyWriteElement name)
  pure $ (, x) e { weDoc                  = d
                 , weExtensions           = reverse $ weExtensions e
                 , weImports              = reverse $ weImports e
                 , weProvides             = reverse $ weProvides e
                 , weUndependableProvides = reverse $ weUndependableProvides e
                 , weSourceDepends        = reverse $ weSourceDepends e
                 , weDepends              = reverse $ weDepends e
                 }

emptyWriteElement :: Text -> WriteElement
emptyWriteElement name = WriteElement { weName                 = name
                                      , weExtensions           = []
                                      , weImports              = []
                                      , weDoc                  = const mempty
                                      , weProvides             = []
                                      , weUndependableProvides = []
                                      , weSourceDepends        = []
                                      , weDepends              = []
                                      , weBootElement          = Nothing
                                      }

----------------------------------------------------------------
-- Using types
----------------------------------------------------------------

toHsType :: Type -> WE (Doc ())
toHsType = toHsTypePrec (-1)

toHsTypePrec :: Int -> Type -> WE (Doc ())
toHsTypePrec prec t = case H.toHsTypePrec prec t of
  Left es -> WE . lift $ do
    _ <- forV es $ \e -> throwError (prettySpecError e)
    throwError ""
  Right (d, (is, es)) -> do
    tellImports' is
    traverse_ tellExtension es
    tellDepends (typeDepends t)
    pure d

makeSourceDepends
  :: [HaskellName]
  -- ^ Exceptions
  -> WE a
  -> WE a
makeSourceDepends exceptions = WE . mapStateT (fmap (fmap f)) . unWE
  where
    f we =
      let (newDepends, newSourceDepends) =
              partition ((`elem` exceptions) . unGuarded) (weDepends we)
      in  we { weDepends       = newDepends
             , weSourceDepends = newSourceDepends <> weSourceDepends we
             }

----------------------------------------------------------------
-- Adding elements to the write element
----------------------------------------------------------------

-- | Set the write element name
tellBootElem :: WriteElement -> WE ()
tellBootElem boot = WE $ gets weBootElement >>= \case
  Nothing -> modify' (\e -> e { weBootElement = Just boot })
  Just _  -> lift $ throw "Write element already has a boot element"

-- | Tells an unguarded import
tellImport
  :: Text
  -- ^ Module
  -> Text
  -- ^ Value
  -> WE ()
tellImport m v = tellImports [Import m [v]]

-- | Tells an unguarded qualified import
tellQualifiedImport
  :: Text
  -- ^ Module
  -> Text
  -- ^ Value
  -> WE ()
tellQualifiedImport m v = tellImports [QualifiedImport m [v]]

-- | Tells some unguarded imports
tellImports :: [Import] -> WE ()
tellImports = tellImports' . fmap Unguarded

-- | Tells some imports
tellImports' :: [Guarded Import] -> WE ()
tellImports' is =
  WE $ modify' (\e -> e { weImports = is <> weImports e })

tellExport :: Export -> WE ()
tellExport ex = WE $ modify' (\e -> e { weProvides = Unguarded ex : weProvides e })

tellUndependableExport :: Export -> WE ()
tellUndependableExport ex = WE $ modify'
  (\e -> e { weUndependableProvides = Unguarded ex : weUndependableProvides e })

tellDepend :: HaskellName -> WE ()
tellDepend d = tellDepends [d]

tellGuardedDepend :: Guarded HaskellName -> WE ()
tellGuardedDepend d = WE $ modify' (\e -> e { weDepends = d : weDepends e })

tellDepends :: [HaskellName] -> WE ()
tellDepends ds = WE $ modify' (\e -> e { weDepends = (Unguarded <$> ds) <> weDepends e })

tellSourceDepend :: HaskellName -> WE ()
tellSourceDepend d =
  WE $ modify' (\e -> e { weSourceDepends = Unguarded d : weSourceDepends e })

tellExtension :: Text -> WE ()
tellExtension ex =
  WE $ modify' (\e -> e { weExtensions = ex : weExtensions e })

----------------------------------------------------------------
-- TODO: Remove
----------------------------------------------------------------

toV :: Write a -> Validation [SpecError] a
toV = eitherToValidation . toE

toE :: Write a -> Either [SpecError] a
toE = first (fmap Other) . flip runReaderT emptyLookup . unWrite

toVLookup :: Lookup -> Write a -> Validation [SpecError] a
toVLookup l = eitherToValidation . toELookup l

toELookup :: Lookup -> Write a -> Either [SpecError] a
toELookup l = first (fmap Other) . flip runReaderT l . unWrite

