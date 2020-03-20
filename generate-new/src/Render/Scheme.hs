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
import           Render.SpecInfo

schemeType
  :: (HasErr r, HasRenderParams r, Show a, HasSpecInfo r)
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
    Returned     _                 -> pure Nothing
    InOutCount   s                 -> schemeType s
    Custom       CustomScheme {..} -> Just <$> csType
    ElidedCustom _                 -> pure Nothing

schemeTypePositive
  :: forall r a
   . (HasErr r, HasRenderParams r, Show a, HasSpecInfo r)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypePositive s = case s of
  Returned   t -> schemeType t
  InOutCount t -> schemeType t
  _            -> pure Nothing

isUnivalued :: MarshalScheme a -> Bool
isUnivalued = \case
  ElidedUnivalued _ -> True
  _                 -> False

isInOutCount :: MarshalScheme a -> Bool
isInOutCount = \case
  InOutCount _ -> True
  _            -> False
