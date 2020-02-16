{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Struct
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Text.InterpolatedString.Perl6.Unindented
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances  ( )
import           Language.Haskell.TH.Syntax     ( lift )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V
import           Spec.Parse
import           Foreign.C.Types
import           Foreign.Ptr

import           CType
import           Haskell                       as H
import           Marshal
import           Marshal.Scheme
import           Error
import           Render.Utils

data RenderParams = RenderParams
  { mkTyName :: Text -> Text
  , mkConName :: Text -> Text
  , mkMemberName :: Text -> Text
  }

renderStruct
  :: (HasErr r, Member (Reader RenderParams) r)
  => MarshaledStruct
  -> Sem r (Doc ())
renderStruct MarshaledStruct {..} = do
  RenderParams {..} <- ask
  ms                <- V.mapMaybe id <$> traverseV renderStructMember msMembers
  pure [qqi|
    data {mkTyName msName} = {mkConName msName}
      {braceList ms}
    |]

renderStructMember
  :: (HasErr r, Member (Reader RenderParams) r)
  => MarshaledStructMember
  -> Sem r (Maybe (Doc ()))
renderStructMember MarshaledStructMember {..} = do
  let StructMember {..} = msmStructMember
  RenderParams {..} <- ask
  fmap (\t -> [qqi|{mkMemberName smName} :: {renderType t}|])
    <$> schemeType msmScheme

schemeType
  :: (HasErr r, Member (Reader RenderParams) r, Show a)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeType s = do
  RenderParams {..} <- ask
  case s of
    Unit                  -> pure $ Just (ConT ''())
    Preserve cType        -> Just <$> cToHsType DoPreserve cType
    Normal   cType        -> Just <$> cToHsType DoNotPreserve cType
    ElidedLength os rs    -> pure Nothing
    ElidedUnivalued value -> pure Nothing
    ElidedVoid            -> pure Nothing
    VoidPtr               -> pure $ Just $(lift =<< [t|Ptr ()|])
    ByteString            -> pure $ Just $ ConT ''ByteString
    Maybe  e              -> fmap (ConT ''Maybe `AppT`) <$> schemeType e
    Vector e              -> fmap (ConT ''V.Vector `AppT`) <$> schemeType e
    EitherWord32 e ->
      fmap (\t -> ConT ''Either :@ ConT ''Word32 :@ (ConT ''V.Vector :@ t))
        <$> schemeType e
    Tupled n e ->
      fmap
          (\t -> foldl' AppT
                        (ConT (tupleTypeName (fromIntegral n)))
                        (replicate (fromIntegral n) t)
          )
        <$> schemeType e
    Returned e -> throw $ "unhandled scheme " <> show s

data Preserve = DoNotPreserve | DoPreserve

cToHsType :: HasErr r => Preserve -> CType -> Sem r H.Type
cToHsType preserve = \case
  Int -> pure $ case preserve of
    DoPreserve    -> ConT ''CInt
    DoNotPreserve -> ConT ''Int32
  Float -> pure $ case preserve of
    DoPreserve    -> ConT ''CFloat
    DoNotPreserve -> ConT ''Float
  Ptr _ p -> do
    t <- cToHsType preserve p
    pure $ ConT ''Ptr :@ t
  TypeName n -> pure $ ConT . mkName . toString $ n
  c          -> throw $ "Unable to get Haskell type for: " <> show c
