{-# language TemplateHaskellQuotes #-}
module Render.Scheme
  where

import qualified Data.Vector                   as V
import           Foreign.Ptr
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances  ( )
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( lift )

import           Error
import           Haskell                       as H
import           Marshal.Scheme
import           Render.Element
import           Render.SpecInfo
import           Render.Type

schemeTypeNegative
  :: (HasErr r, HasRenderParams r, Show a, HasSpecInfo r)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypeNegative s = do
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
    Maybe e           -> fmap (ConT ''Maybe :@) <$> schemeTypeNegative e
    Vector _ e        -> fmap (ConT ''V.Vector :@) <$> schemeTypeNegative e
    EitherWord32 e ->
      fmap (\t -> ConT ''Either :@ ConT ''Word32 :@ (ConT ''V.Vector :@ t))
        <$> schemeTypeNegative e
    Tupled n e ->
      fmap (foldl' (:@) (TupleT (fromIntegral n)) . replicate (fromIntegral n))
        <$> schemeTypeNegative e
    WrappedStruct n ->
      pure . Just $ ConT (typeName (TyConName "SomeStruct")) :@ ConT
        (typeName (mkTyName n))
    Returned     _                 -> pure Nothing
    InOutCount   s                 -> schemeTypeNegative s
    Custom       CustomScheme {..} -> Just <$> csType
    ElidedCustom _                 -> pure Nothing

schemeTypePositive
  :: forall r a
   . (HasErr r, HasRenderParams r, Show a, HasSpecInfo r)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypePositive s = case s of
  Returned   t -> schemeTypeNegative t
  InOutCount t -> schemeTypeNegative t
  _            -> pure Nothing

isInOutCount :: MarshalScheme a -> Bool
isInOutCount = \case
  InOutCount _ -> True
  _            -> False
