{-# language TemplateHaskellQuotes #-}
module Render.Type
  ( Preserve(..)
  , cToHsType
  , cToHsTypeWithHoles
  , cToHsTypeQuantified
  , namedTy
  )
where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , State
                                                , modify'
                                                , get
                                                , put
                                                , runState
                                                , Type
                                                )
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances  ( )
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Polysemy.Reader
import           Foreign.C.Types
import           Foreign.Ptr
import qualified Data.Vector.Storable.Sized    as VSS

import           Render.SpecInfo
import           Spec.Types
import           CType
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Type.Preserve

-- | The same as 'cToHsType' except type variables are written as @_@
cToHsTypeWithHoles
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsTypeWithHoles preserve t = runInputList holes $ cToHsType' preserve t

-- | The same as 'cToHsType' except type variables are quantified with forall.
cToHsTypeQuantified
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsTypeQuantified preserve t = do
  let nextVar = do
        (u, i) <- get
        let name = mkName [i]
        case i of
          'q' -> pure Nothing
          _ -> do
            put (name : u, succ i)
            pure $ Just (VarT name)
  ((usedVarNames, _), t) <-
    runState ([], 'a') . runInputSem nextVar $ cToHsType' preserve t
  pure $ ForallT (PlainTV <$> reverse usedVarNames) [] t

cToHsType
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsType preserve t = runInputList allVars $ cToHsType' preserve t

cToHsType'
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, Member NextVar r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsType' preserve t = do
  RenderParams {..} <- ask
  case mkHsTypeOverride preserve t of
    Just h  -> pure h
    Nothing -> do
      t' <- r
      pure $ case preserve of
        DoNotPreserve -> maybe t' itType (mkIdiomaticType t')
        DoPreserve    -> t'
        DoLower       -> t'

 where
  r :: Sem r H.Type
  r = case t of
    Void -> case preserve of
      DoLower       -> pure (TupleT 0)
      DoPreserve    -> pure (TupleT 0)
      DoNotPreserve -> throw "Getting the unpreserved haskell type for void"
    Int    -> pure $ ConT ''CInt
    Float  -> pure $ ConT ''CFloat
    Double -> pure $ ConT ''CDouble
    Char   -> case preserve of
      DoLower    -> pure $ ConT ''CChar
      DoPreserve -> pure $ ConT ''CChar
      DoNotPreserve ->
        throw
          "Getting the unpreserved haskell type for char. This case should be implemented if this char is not better represented by a bytestring"
    Ptr _ Void -> pure $ ConT ''Ptr :@ TupleT 0
    Ptr _ p    -> do
      t' <- cToHsType' preserve p
      pure $ ConT ''Ptr :@ t'
    Array _ (NumericArraySize n) e -> do
      e' <- cToHsType' preserve e
      let arrayTy = ConT ''VSS.Vector :@ LitT (NumTyLit (fromIntegral n)) :@ e'
      pure $ case preserve of
        DoLower -> ConT ''Ptr :@ arrayTy
        _       -> arrayTy
    Array _ (SymbolicArraySize n) e -> do
      RenderParams {..} <- ask
      e'                <- cToHsType' preserve e
      let arrayTy = ConT ''VSS.Vector :@ ConT (typeName (mkTyName n)) :@ e'
      pure $ case preserve of
        DoLower -> ConT ''Ptr :@ arrayTy
        _       -> arrayTy
    TypeName "uint8_t"  -> pure $ ConT ''Word8
    TypeName "uint16_t" -> pure $ ConT ''Word16
    TypeName "uint32_t" -> pure $ ConT ''Word32
    TypeName "uint64_t" -> pure $ ConT ''Word64
    TypeName "int8_t"   -> pure $ ConT ''Int8
    TypeName "int16_t"  -> pure $ ConT ''Int16
    TypeName "int32_t"  -> pure $ ConT ''Int32
    TypeName "int64_t"  -> pure $ ConT ''Int64
    TypeName "size_t"   -> pure $ ConT ''CSize
    TypeName n          -> do
      RenderParams {..} <- ask
      let con = ConT . typeName . mkTyName $ n
      getStruct n >>= \case
        Just s | not (V.null (sExtendedBy s)) -> do
          var <- nextVar
          pure $ con :@ var
        _ -> pure con
    Proto ret ps -> do
      retTy <- cToHsType' preserve ret
      pTys  <- forV ps $ \(n, c) -> do
        t' <- cToHsType' preserve c
        pure $ case n of
          Nothing   -> t'
          Just name -> namedTy name t'
      pure $ foldr (~>) (ConT ''IO :@ retTy) pTys

-- TODO: Remove vulkan specific stuff here
namedTy :: Text -> H.Type -> H.Type
namedTy name ty =
  let lowerName = Just . T.toLower . T.pack . nameBase
      tyName    = case ty of
        ConT n                       -> lowerName n
        ConT n :@ VarT _             -> lowerName n
        ConT w :@ ConT n | w == ''Ptr -> lowerName n
        _                            -> Nothing
  in  case tyName of
        Just n | T.toLower name `T.isInfixOf` n -> ty
        -- If the name contains no more information than the type, don't include
        -- it
        _ -> InfixT (LitT (StrTyLit (toString name)))
                    (typeName (TyConName ":::"))
                    ty

type NextVar = Input (Maybe Type)

nextVar :: (HasErr r, Member NextVar r) => Sem r Type
nextVar =
  input @(Maybe Type) >>= \case
    Nothing -> throw "Run out of variables"
    Just v ->  pure v

-- 'r' is used elsewhere for return types
allVars :: [Type]
allVars = (\c -> VarT (mkName [c])) <$> ['a' .. 'q']

holes :: [Type]
holes = repeat WildCardT
