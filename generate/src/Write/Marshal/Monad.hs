{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards  #-}

module Write.Marshal.Monad
  where

import           Control.Monad.Except
import           Control.Monad.Writer.Strict
import           Data.Function
import           Data.Functor
import           Data.Text                   (Text)
import           Data.Text.Prettyprint.Doc
import           Prelude                     hiding (Enum)

import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Savvy.Type.Haskell     as H

import           Write.Element               hiding (TypeName)

----------------------------------------------------------------
-- Monad for wrapping
----------------------------------------------------------------

type Extension = Text
type Constraint = Text
type WriteState =
  ( [Import]
  , ( [Guarded Export]
    , [Guarded Export]
    )
  , ( [Guarded HaskellName] -- Depends
    , [Guarded HaskellName] -- Source depends
    )
  , [Extension]
  , [Constraint]
  )
-- TODO, better type

getConstraints :: WriteState -> [Constraint]
getConstraints (_,_,_,_,cs) = cs

type WrapM = WriterT WriteState (Except [SpecError])

runWrap
  :: WrapM a
  -> Either [SpecError] (a, WriteState)
runWrap = runExcept . runWriterT

wrapMToWriteElements
  :: Text
  -- ^ Name
  -> Maybe WriteElement
  -- ^ Boot element
  -> WrapM (DocMap -> Doc ())
  -> Either [SpecError] WriteElement
wrapMToWriteElements weName weBootElement w = do
  (weDoc, (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <-
    either (throwError . fmap (WithContext weName)) pure (runWrap w)
  pure WriteElement {..}

tellImport
  :: Text
  -- ^ Module
  -> Text
  -- ^ Value
  -> WrapM ()
tellImport m v = tell ([Import m [v]], mempty, mempty, mempty, mempty)

tellQualifiedImport
  :: Text
  -- ^ Module
  -> Text
  -- ^ Value
  -> WrapM ()
tellQualifiedImport m v =
  tell ([QualifiedImport m [v]], mempty, mempty, mempty, mempty)

tellImports
  :: [Import]
  -> WrapM ()
tellImports is = tell (is, mempty, mempty, mempty, mempty)

tellExport
  :: Guarded Export
  -> WrapM ()
tellExport e = tell (mempty, ([e], mempty), mempty, mempty, mempty)

tellUndependableExport
  :: Guarded Export
  -> WrapM ()
tellUndependableExport e = tell (mempty, (mempty, [e]), mempty, mempty, mempty)

tellDepend
  :: Guarded HaskellName
  -> WrapM ()
tellDepend d = tell (mempty, mempty, ([d], mempty), mempty, mempty)

tellDepends
  :: [Guarded HaskellName]
  -> WrapM ()
tellDepends ds = tell (mempty, mempty, (ds, mempty), mempty, mempty)

tellSourceDepend
  :: Guarded HaskellName
  -> WrapM ()
tellSourceDepend d = tell (mempty, mempty, (mempty, [d]), mempty, mempty)

tellExtension
  :: Text
  -> WrapM ()
tellExtension e = tell (mempty, mempty, mempty, [e], mempty)

tellConstraint
  :: Text
  -> WrapM ()
tellConstraint c = tell (mempty, mempty, mempty, mempty, [c])

toHsType :: Type -> WrapM (Doc ())
toHsType t = case H.toHsType t of
  Left es -> throwError es
  Right (d, (is, es)) -> do
    tell (is, mempty, mempty, es, mempty)
    -- TODO: This is a bit of a hack
    tellDepends (Unguarded <$> typeDepends t)
    pure d

