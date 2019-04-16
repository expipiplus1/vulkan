{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Bracket
  ( bracketCommands
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Command
import           Spec.Savvy.Type

import           Write.Element                     hiding ( TypeName )
import qualified Write.Element                 as WE
import           Write.Marshal.Monad
import           Write.Marshal.Util

import Debug.Trace

data Bracket = Bracket
  { bName :: HaskellName
  , bType :: Type
  , bOpen :: Command
  , bClose :: Command
  }
  deriving(Show)

brackets :: [Command] -> [Bracket]
brackets commands =
  let isOpener c@Command{..} = guard ("vkCreate" `T.isPrefixOf` cName) $> c
      isCloser c@Command{..} = guard ("vkDestroy" `T.isPrefixOf` cName) $> c
      isPair co cc
        | Just h <- T.dropPrefix "vkCreate" (cName co)
        , Just h' <- T.dropPrefix "vkDestroy" (cName cc)
        , h == h'
        , Ptr NonConst t <- pType (last (cParameters co))
        , TypeName vn <- t
        = let name = "with" <> dropVk vn
          in Just (Bracket name t co cc)
        | otherwise = Nothing
  in  ps = pairs isOpener isCloser isPair commands

-- | Find pairs of commands which should be called using the bracket pattern
bracketCommands :: [Command] -> Either [SpecError] [((Type, HaskellName), WriteElement)]
bracketCommands commands = do
  let isOpener c@Command{..} = guard ("vkCreate" `T.isPrefixOf` cName) $> c
      isCloser c@Command{..} = guard ("vkDestroy" `T.isPrefixOf` cName) $> c
      isPair co cc
        | Just h <- T.dropPrefix "vkCreate" (cName co)
        , Just h' <- T.dropPrefix "vkDestroy" (cName cc)
        , h == h'
        = Just (CreateSingle co cc)
        | otherwise = Nothing
      ps = pairs isOpener isCloser isPair commands
  for ps $ \p -> do
      let weName        = T.tShow p T.<+> "bracket"
          weBootElement = Nothing
      ((t, weDoc), (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <-
        either (throwError . fmap (WithContext weName))
               pure
               (runWrap $ bracketCommand p)
      pure (t, WriteElement {..})

bracketCommand :: CommandPair -> WrapM ((Type, HaskellName), DocMap -> Doc ())
bracketCommand c@(CreateSingle co cc) = do
  let Ptr NonConst t = traceShowId $ pType (last (cParameters co))
      TypeName vn = t
      n = dropVk vn
      wrapName = "with" <> n
  tellExport (Unguarded (Term wrapName))
  pure $ ((t, WE.TermName wrapName),) $ \_ -> [qci|
    with{n} = _ -- {show c}
  |]

pairs :: (a -> Maybe l) -> (a -> Maybe r) -> (l -> r -> Maybe p) -> [a] -> [p]
pairs l r p xs =
  [ p' | Just l' <- l <$> xs, Just r' <- r <$> xs, Just p' <- [p l' r'] ]
