{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Write.Marshal.Monad
  where

import           Control.Arrow                            ((&&&))
import           Control.Bool
import           Control.Category                         ((>>>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Strict              hiding ((<>))
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Monoid                              (Endo (..))
import qualified Data.MultiMap                            as MultiMap
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Savvy.Type.Haskell                  as H

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Struct
import           Write.Util

----------------------------------------------------------------
-- Monad for wrapping
----------------------------------------------------------------

type Extension = Text
type Constraint = Text
type WriteState = ([Import], [Guarded Export], [Guarded HaskellName], [Extension], [Constraint])

getConstraints :: WriteState -> [Constraint]
getConstraints (_,_,_,_,cs) = cs

type WrapM = WriterT WriteState (Except [SpecError])

runWrap
  :: WrapM a
  -> Either [SpecError] (a, WriteState)
runWrap = runExcept . runWriterT

tellImport
  :: Text
  -- ^ Module
  -> Text
  -- ^ Value
  -> WrapM ()
tellImport m v = tell ([Import m [v]], [], [], [], [])

tellQualifiedImport
  :: Text
  -- ^ Module
  -> Text
  -- ^ Value
  -> WrapM ()
tellQualifiedImport m v = tell ([QualifiedImport m [v]], [], [], [], [])

tellImports
  :: [Import]
  -> WrapM ()
tellImports is = tell (is, [], [], [], [])

tellExport
  :: Guarded Export
  -> WrapM ()
tellExport e = tell ([], [e], [], [], [])

tellDepend
  :: Guarded HaskellName
  -> WrapM ()
tellDepend d = tell ([], [], [d], [], [])

tellDepends
  :: [Guarded HaskellName]
  -> WrapM ()
tellDepends ds = tell ([], [], ds, [], [])

tellExtension
  :: Text
  -> WrapM ()
tellExtension e = tell ([], [], [], [e], [])

tellConstraint
  :: Text
  -> WrapM ()
tellConstraint c = tell ([], [], [], [], [c])

toHsType :: Type -> WrapM (Doc ())
toHsType t = case H.toHsType t of
  Left es -> throwError es
  Right (d, (is, es)) -> do
    tell (is, [], [], es, [])
    -- TODO: This is a bit of a hack
    tellDepends (Unguarded <$> typeDepends t)
    pure d
