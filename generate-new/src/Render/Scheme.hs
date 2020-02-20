{-# language TemplateHaskellQuotes #-}
module Render.Scheme
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances  ( )
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V
import           Foreign.Ptr

import           Haskell                       as H
import           Marshal.Scheme
import           Error
import           Render.Type
import           Render.Element

schemeType
  :: (HasErr r, Member (Reader RenderParams) r, Show a)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeType s = do
  RenderParams {..} <- ask
  case s of
    Unit              -> pure $ Just (ConT ''())
    Preserve cType    -> Just <$> cToHsType DoPreserve cType
    Normal   cType    -> Just <$> cToHsType DoNotPreserve cType
    ElidedLength _ _  -> pure Nothing
    ElidedUnivalued _ -> pure Nothing
    ElidedVoid        -> pure Nothing
    VoidPtr           -> pure . Just $ ConT ''Ptr :@ ConT ''()
    ByteString        -> pure . Just $ ConT ''ByteString
    Maybe  e          -> fmap (ConT ''Maybe :@) <$> schemeType e
    Vector e          -> fmap (ConT ''V.Vector :@) <$> schemeType e
    EitherWord32 e ->
      fmap (\t -> ConT ''Either :@ ConT ''Word32 :@ (ConT ''V.Vector :@ t))
        <$> schemeType e
    Tupled n e ->
      fmap (foldl' (:@) (TupleT (fromIntegral n)) . replicate (fromIntegral n))
        <$> schemeType e
    Returned _ -> pure Nothing

schemeTypePositive
  :: (HasErr r, Member (Reader RenderParams) r, Show a)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypePositive s = do
  RenderParams {..} <- ask
  case s of
    Returned r -> schemeType r
    _          -> pure Nothing
