{-# language TemplateHaskellQuotes #-}
{-# language NoStarIsType #-}
module Render.Type
  ( Preserve(..)
  , ExtensibleStructStyle(..)
  , cToHsType
  , cToHsTypeWrapped
  , cToHsTypeWithHoles
  , cToHsTypeQuantified
  , namedTy
  ) where

import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Foreign.C.Types
import           Foreign.Ptr
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances  ( )
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Relude                  hiding ( State
                                                , Type
                                                , get
                                                , lift
                                                , modify'
                                                , put
                                                , runState
                                                )

import           GHC.TypeNats

import           CType
import           Error
import           Haskell                       as H
import           Render.Element
import           Render.SpecInfo
import           Render.Type.Preserve
import           Spec.Types

-- | The same as 'cToHsType' except type variables are written as @_@
cToHsTypeWithHoles
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsTypeWithHoles = cToHsType' (Applied (pure WildCardT))

-- | The same as 'cToHsType' except extensible structs are wrapped in @SomeStruct@
cToHsTypeWrapped
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsTypeWrapped = cToHsType' Wrapped

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
          _   -> do
            put (name : u, succ i)
            pure $ Just (VarT name)
  ((usedVarNames, _), t) <-
    runState ([], 'a') . runInputSem nextVar $ cToHsType' (Applied getVar)
                                                          preserve
                                                          t
  pure $ ForallT (PlainTV <$> reverse usedVarNames) [] t

cToHsType
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsType preserve t =
  runInputList allVars $ cToHsType' (Applied getVar) preserve t

cToHsType'
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => ExtensibleStructStyle r
  -> Preserve
  -> CType
  -> Sem r H.Type
cToHsType' structStyle preserve t = do
  RenderParams {..} <- input
  case mkHsTypeOverride structStyle preserve t of
    Just h  -> h
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
      t' <- cToHsType' structStyle preserve p
      pure $ ConT ''Ptr :@ t'
    Array _ s e -> do
      e' <- cToHsType' structStyle preserve e
      s' <- arraySizeType s
      let arrayTy =
            ConT (mkName "Graphics.Vulkan.CStruct.Utils.FixedArray") :@ s' :@ e'
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
      RenderParams {..} <- input
      let con = ConT . typeName . mkTyName $ n
      getStruct n >>= \case
        Just s | not (V.null (sExtendedBy s)) -> case structStyle of
          Applied getVar -> do
            var <- getVar
            pure $ con :@ var
          Wrapped -> pure $ ConT (mkName "SomeStruct") :@ con
        _ -> pure con
    Proto ret ps -> do
      retTy <- cToHsType' structStyle preserve ret
      pTys  <- forV ps $ \(n, c) -> do
        t' <- cToHsType' structStyle preserve c
        pure $ case n of
          Nothing   -> t'
          Just name -> namedTy name t'
      pure $ foldr (~>) (ConT ''IO :@ retTy) pTys
    Bitfield _ _ -> throw "TODO Bitfields"

arraySizeType :: HasRenderParams r => ArraySize -> Sem r H.Type
arraySizeType = \case
  NumericArraySize  n -> pure $ LitT (NumTyLit (fromIntegral n))
  SymbolicArraySize n -> do
    RenderParams {..} <- input
    pure $ ConT (typeName (mkTyName n))
  MultipleArraySize a b -> do
    a <- arraySizeType (NumericArraySize a)
    b <- arraySizeType b
    pure $ InfixT a ''(*) b

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

getVar :: (HasErr r, Member NextVar r) => Sem r Type
getVar =
  input @(Maybe Type) >>= \case
    Nothing -> throw "Run out of variables"
    Just v ->  pure v

-- 'r' is used elsewhere for return types
allVars :: [Type]
allVars = (\c -> VarT (mkName [c])) <$> ['a' .. 'q']
