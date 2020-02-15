{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Command
  where

import           Control.Arrow                            ( (&&&) )
import           Control.Bool
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Strict       hiding ( (<>) )
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor
import qualified Data.Map                      as Map
import           Data.List                                ( partition )
import           Data.Maybe
import qualified Data.MultiMap                 as MultiMap
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Handle
import           Spec.Savvy.Type

import           Write.Element                     hiding ( TypeName )
import qualified Write.Element                 as WE
import           Write.Monad
import           Write.Util                               ( documentUp
                                                          , Documentee(..)
                                                          )
import           Write.Marshal.Util
import           Write.Marshal.Wrap
import           Write.Marshal.Struct
import           Write.Marshal.Type
import           Write.Marshal.Scheme
import           Write.Marshal.Marshallable
import           Write.Marshal.ToC
import           Write.Monad

marshalCommand :: Command -> Write [WriteElement]
marshalCommand command = withContext (cName command) $ do
  schemes <- marshalCommandParams command
  let (negativeSchemes, positiveSchemes) =
        partition (isNegative . msPosition) schemes
      rv = ReturnValue (cReturnType command)
  rScheme <- typeToScheme rv =<< marshalRVType command rv
  mainWE  <- runWE (cName command T.<+> "wrapper") $ do
    tellExport (Term (dropVk (cName command)))
    sig     <- typeSig command rScheme negativeSchemes positiveSchemes
    binding <- marshalledBinding command rScheme negativeSchemes positiveSchemes
    pure $ \docMap -> vsep [sig docMap, binding]
  aliasWEs <- traverseV
    (writeAlias command rScheme negativeSchemes positiveSchemes)
    (cAliases command)
  pure $ mainWE : aliasWEs

marshalledBinding
  :: Command
  -> MarshalScheme ReturnValue
  -> [MarshalScheme Parameter]
  -> [MarshalScheme Parameter]
  -> WE (Doc ())
marshalledBinding Command {..} r negativeSchemes positiveSchemes = do
  let pats            = marshalledNameDoc . msParam <$> negativeSchemes
  pure $ pretty (dropVk cName) <+> hsep pats <+> "=" <+> "do" <> line <> indent
    2
    ("pure _")

typeSig
  :: Command
  -> MarshalScheme ReturnValue
  -> [MarshalScheme Parameter]
  -> [MarshalScheme Parameter]
  -> WE (DocMap -> Doc ())
typeSig c@Command {..} rv negativeSchemes positiveSchemes = do
  t <- marshalledCommandType c rv negativeSchemes positiveSchemes
  pure $ \docs -> pretty (dropVk cName) <> line <> t docs

marshalledCommandType
  :: Command
  -> MarshalScheme ReturnValue
  -> [MarshalScheme Parameter]
  -> [MarshalScheme Parameter]
  -> WE (DocMap -> Doc ())
marshalledCommandType Command {..} rv negativeSchemes positiveSchemes = do
  tDocs <-
    sequence
    . catMaybes
    . fmap (marshalledParamTypeWithDoc cName)
    $ negativeSchemes
  rDocs <-
    let rvM = case msMarshalledType rv of
          Elided      -> Nothing
          Present t _ -> Just t
    in  sequence
        . (toList rvM ++)
        . catMaybes
        . fmap (marshalledParamType cName)
        $ positiveSchemes
  let rDoc = "IO" <+> tupled rDocs
  pure $ \docs ->
    let ls = zipWith (<+>) ("::" : repeat "->") (sequence tDocs docs ++ [rDoc])
    in  indent 2 (vcat ls)

marshalledParamTypeWithDoc
  :: Text -> MarshalScheme Parameter -> Maybe (WE (DocMap -> Doc ()))
marshalledParamTypeWithDoc parentName t =
  let p = msParam t
  in
    case marshalledParamType parentName t of
      Nothing -> Nothing
      Just t  -> Just $ do
        tDoc <- t
        pure $ \getDoc ->
          vcat [tDoc, documentUp getDoc (Nested parentName (pName p))]

marshalledParamType :: Text -> MarshalScheme Parameter -> Maybe (WE (Doc ()))
marshalledParamType parentName t =
  let p = msParam t
  in  case msMarshalledType t of
        Elided      -> Nothing
        Present t _ -> Just $ do
          tDoc <- t
          tellDepend (WE.TypeName "(:::)")
          pure $ "\"" <> marshalledNameDoc p <> "\" :::" <+> tDoc

----------------------------------------------------------------
-- Aliases
----------------------------------------------------------------

writeAlias
  :: Command
  -- ^ The original command
  -> MarshalScheme ReturnValue
  -> [MarshalScheme Parameter]
  -> [MarshalScheme Parameter]
  -> Text
  -- ^ The alias name
  -> Write WriteElement
writeAlias c@Command {..} rScheme negativeSchemes positiveSchemes name =
  runWE (name T.<+> "alias" T.<+> cName) $ do
    tellExport (Term (dropVk name))
    tellDepend (TermName (dropVk cName))
    t <- marshalledCommandType c rScheme negativeSchemes positiveSchemes
    pure $ \docMap ->  [qci|
      {dropVk name} :: {t docMap}
      {dropVk name} = {dropVk cName}
    |]
