{-# language TemplateHaskellQuotes #-}
module Render.Scheme
  ( schemeTypeNegative
  , schemeTypeNegativeIgnoreContext
  , schemeTypeNegativeWithContext
  , schemeTypePositive
  , schemeTypePositiveWithContext
  , isInOutCount
  ) where

import qualified Data.Vector                   as V
import           Foreign.Ptr
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances  ( )
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( lift )

import           CType                          ( CType )
import           Error
import           Haskell                       as H
import           Marshal.Scheme
import           Render.Element
import           Render.SpecInfo
import           Render.Type

schemeTypeNegativeWithContext
  :: (HasErr r, HasRenderParams r, Show a, HasSpecInfo r, HasContextState r)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypeNegativeWithContext = context "schemeTypeNegativeWithContext"
  . schemeTypeNegative' cToHsTypeWithContext

schemeTypeNegative
  :: (HasErr r, HasRenderParams r, Show a, HasSpecInfo r)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypeNegative =
  context "schemeTypeNegative" . schemeTypeNegative' cToHsType

-- TODO: Remove
schemeTypeNegativeIgnoreContext
  :: (HasErr r, HasRenderParams r, Show a, HasSpecInfo r)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypeNegativeIgnoreContext s =
  let thirdOfThree (_, _, x) = x
  in  thirdOfThree <$> runRenderTypeContext (schemeTypeNegativeWithContext s)

schemeTypeNegative'
  :: (HasErr r, HasRenderParams r, Show a, HasSpecInfo r)
  => (Preserve -> CType -> Sem r H.Type)
  -> MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypeNegative' toHs s = do
  RenderParams {..} <- input
  case s of
    Unit              -> pure $ Just (ConT ''())
    Preserve cType    -> Just <$> toHs DoPreserve cType
    Normal   cType    -> Just <$> toHs DoNotPreserve cType
    Length cType _ _  -> Just <$> toHs DoNotPreserve cType
    ElidedLength{}    -> pure Nothing
    ElidedUnivalued _ -> pure Nothing
    ElidedVoid        -> pure Nothing
    VoidPtr           -> pure . Just $ ConT ''Ptr :@ ConT ''()
    ByteString        -> pure . Just $ ConT ''ByteString
    Maybe e           -> fmap (ConT ''Maybe :@) <$> schemeTypeNegative' toHs e
    Vector _ e        -> fmap (ConT ''V.Vector :@) <$> schemeTypeNegative' toHs e
    EitherWord32 e ->
      fmap (\t -> ConT ''Either :@ ConT ''Word32 :@ (ConT ''V.Vector :@ t))
        <$> schemeTypeNegative' toHs e
    Tupled n e ->
      fmap (foldl' (:@) (TupleT (fromIntegral n)) . replicate (fromIntegral n))
        <$> schemeTypeNegative' toHs e
    WrappedStruct n ->
      pure . Just $ ConT (typeName (TyConName "SomeStruct")) :@ ConT
        (typeName (mkTyName n))
    WrappedChildStruct n@"XrCompositionLayerBaseHeader" ->
      pure
        .  Just
        $  ConT (typeName (TyConName "SomeChild"))
        :@ (ConT (typeName (mkTyName n)) :@ PromotedNilT)
    WrappedChildStruct n ->
      pure . Just $ ConT (typeName (TyConName "SomeChild")) :@ ConT
        (typeName (mkTyName n))
    Returned     _                 -> pure Nothing
    InOutCount   s                 -> schemeTypeNegative' toHs s
    Custom       CustomScheme {..} -> Just <$> csType
    ElidedCustom _                 -> pure Nothing

schemeTypePositiveWithContext
  :: (HasErr r, HasRenderParams r, Show a, HasSpecInfo r, HasContextState r)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypePositiveWithContext =
  context "schemeTypePositiveWithContext"
    . negateTypeContext
    . schemeTypePositive' schemeTypeNegativeWithContext

schemeTypePositive
  :: forall r a
   . (HasErr r, HasRenderParams r, Show a, HasSpecInfo r)
  => MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypePositive =
  context "schemeTypePositive" . schemeTypePositive' schemeTypeNegative

schemeTypePositive'
  :: forall r a
   . (MarshalScheme a -> Sem r (Maybe H.Type))
  -> MarshalScheme a
  -> Sem r (Maybe H.Type)
schemeTypePositive' stn s = case s of
  Returned   t -> stn t
  InOutCount t -> stn t
  _            -> pure Nothing

isInOutCount :: MarshalScheme a -> Bool
isInOutCount = \case
  InOutCount _ -> True
  _            -> False
