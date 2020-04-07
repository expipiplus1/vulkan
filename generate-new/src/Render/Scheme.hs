{-# language TemplateHaskellQuotes #-}
module Render.Scheme
  where

import           Relude                  hiding ( lift )
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances  ( )
import           Polysemy
import           Polysemy.Input
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
  RenderParams {..} <- input
  case s of
    Unit              -> pure $ Just (ConT ''())
    Preserve cType    -> Just <$> cToHsType DoPreserve cType
    Normal   cType    -> Just <$> cToHsType DoNotPreserve cType
    ElidedLength{}    -> pure Nothing
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
    WrappedStruct n ->
      pure . Just $ ConT (typeName (TyConName "SomeStruct")) :@ ConT
        (typeName (mkTyName n))
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

isInOutCount :: MarshalScheme a -> Bool
isInOutCount = \case
  InOutCount _ -> True
  _            -> False
