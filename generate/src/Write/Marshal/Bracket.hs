{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Bracket
  ( Bracket(..)
  , extractBrackets
  , bracketCommand
  , insertBracketDependency
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

extractBrackets :: [Command] -> [Bracket]
extractBrackets commands =
  let isOpener c@Command{..} = guard ("vkCreate" `T.isPrefixOf` cName) $> c
      isCloser c@Command{..} = guard ("vkDestroy" `T.isPrefixOf` cName) $> c
      isPair co cc
        | Just h <- T.dropPrefix "vkCreate" (cName co)
        , Just h' <- T.dropPrefix "vkDestroy" (cName cc)
        , h == h'
        , Ptr NonConst t <- pType (last (cParameters co))
        , TypeName vn <- t
        = let name = TermName ("with" <> dropVkType vn)
          in Just (Bracket name t co cc)
        | otherwise = Nothing
  in  pairs isOpener isCloser isPair commands

insertBracketDependency :: [Bracket] -> WriteElement -> WriteElement
insertBracketDependency bs we@WriteElement {..} =
  let weDepends' =
        weDepends
          ++ [ p $> bName
             | Bracket {..} <- bs
             , p            <- weProvides
             , TypeName n   <- [bType]
             , dropVkType n == unHaskellName (unExport (unGuarded p))
             ]
  in  WriteElement {weDepends = weDepends', ..}

-- | Find pairs of commands which should be called using the bracket pattern
bracketCommand :: Bracket -> Either [SpecError] WriteElement
bracketCommand b = do
  let weName        = T.tShow (bName b)
      weBootElement = Nothing
  (weDoc, (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <-
    either (throwError . fmap (WithContext weName))
           pure
           (runWrap $ bracketCommand' b)
  pure WriteElement {..}

bracketCommand' :: Bracket -> WrapM (DocMap -> Doc ())
bracketCommand' Bracket{..} = do
  tellExport (Unguarded (WithoutConstructors bName))
  tellImport "Control.Exception" "bracket"
  pure $ \_ -> [qci|
    {unHaskellName bName} :: CreateInfo -> Maybe AllocationCallbacks -> (t -> IO a) -> IO a
    {unHaskellName bName} createInfo allocationCallbacks =
      bracket
        ({cName bOpen} createInfo allocationCallbacks)
        (`{cName bClose}` allocationCallbacks)
  |]

pairs :: (a -> Maybe l) -> (a -> Maybe r) -> (l -> r -> Maybe p) -> [a] -> [p]
pairs l r p xs =
  [ p' | Just l' <- l <$> xs, Just r' <- r <$> xs, Just p' <- [p l' r'] ]
