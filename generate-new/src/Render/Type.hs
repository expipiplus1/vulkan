{-# language TemplateHaskellQuotes #-}
{-# language NoStarIsType #-}
module Render.Type
  ( Preserve(..)
  , ExtensibleStructStyle(..)
  , cToHsType
  , cToHsTypeWithContext
  , cToHsTypeWrapped
  , cToHsTypeWithHoles
  , cToHsTypeQuantified
  , namedTy
  , HasContextState
  , runRenderTypeContext
  , negateTypeContext
  , ConstrainedVar(..)
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
                                                , gets
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
import           Render.Utils                  ( plainTVcompat )
import           Spec.Types

-- | The same as 'cToHsType' except type variables are written as @_@
cToHsTypeWithHoles
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsTypeWithHoles = runNoContext .: cToHsType' UnwrappedHole

-- | The same as 'cToHsType' except extensible structs are wrapped in @SomeStruct@
cToHsTypeWrapped
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsTypeWrapped = runNoContext .: cToHsType' Wrapped

-- | The same as 'cToHsType' except type variables are quantified with forall.
cToHsTypeQuantified
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsTypeQuantified preserve t = do
  (negNames, posNames, t) <- runRenderTypeContext
    $ cToHsType' Unwrapped preserve t
  pure $ ForallT (plainTVcompat . cVarName <$> (negNames <> posNames)) [] t

cToHsTypeWithContext
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, HasContextState r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsTypeWithContext = cToHsType' Unwrapped

cToHsType
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsType preserve t = runNoContext $ cToHsType' Unwrapped preserve t

cToHsType'
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, HasContextState r)
  => ExtensibleStructStyle r
  -> Preserve
  -> CType
  -> Sem r H.Type
cToHsType' structStyle preserve t = do
  RenderParams {..} <- input
  case mkHsTypeOverride getVar structStyle preserve t of
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
      -- TODO: restore the error here
      DoNotPreserve ->
        throw
          "Getting the unpreserved haskell type for char. This case should be implemented if this char is not better represented by a bytestring"
    Ptr _ Void -> pure $ ConT ''Ptr :@ TupleT 0
    Ptr _ p    -> do
      t' <- cToHsType' structStyle preserve p
      pure $ ConT ''Ptr :@ t'
    Array _ s e -> do
      RenderParams {..} <- input
      e'                <- cToHsType' structStyle preserve e
      s'                <- arraySizeType s
      let arrayTy =
            ConT (mkName (T.unpack modulePrefix <> ".CStruct.Utils.FixedArray"))
              :@ s'
              :@ e'
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
      -- TODO: Remove
      let con = case n of
            "XrCompositionLayerBaseHeader" ->
              (ConT . typeName . mkTyName $ n) :@ PromotedNilT
            _ -> ConT . typeName . mkTyName $ n
      getStruct n >>= \case
        Just s | not (V.null (sInheritedBy s)) -> case structStyle of
          Unwrapped     -> VarT <$> getVar (Inherits con)
          UnwrappedHole -> pure WildCardT
          Wrapped       -> pure $ ConT (mkName "SomeChild") :@ con
        Just s | not (V.null (sExtendedBy s)) -> case structStyle of
          Unwrapped -> do
            var <- VarT <$> getVar (Extends con)
            pure $ con :@ var
          UnwrappedHole -> pure $ con :@ WildCardT
          Wrapped       -> pure $ ConT (mkName "SomeStruct") :@ con
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

data ContextState = ContextState
  { csNegativeVars :: [ConstrainedVar]
  , csPositiveVars :: [ConstrainedVar]
  , csNextVars     :: [Name]
  }

type HasContextState r = Member (State ContextState) r

initialContextState :: ContextState
initialContextState = ContextState mempty mempty allVars

negateTypeContext :: HasContextState r => Sem r a -> Sem r a
negateTypeContext a = do
  let swap cs = cs { csNegativeVars = csPositiveVars cs
                   , csPositiveVars = csNegativeVars cs
                   }
  modify' swap
  r <- a
  modify' swap
  pure r

getVar
  :: (HasErr r, HasContextState r) => (Name -> ConstrainedVar) -> Sem r Name
getVar c = gets csNextVars >>= \case
  []       -> throw "Ran out of variables"
  (v : vs) -> do
    modify'
      (\cs -> cs { csNextVars = vs, csNegativeVars = c v : csNegativeVars cs })
    pure v

-- 'r' is used elsewhere for return types
allVars :: [Name]
allVars = (\c -> mkName [c]) <$> ['a' .. 'q']

-- | Defaults to negative position for vars
runRenderTypeContext
  :: HasErr r
  => Sem (State ContextState : r) a
  -> Sem r ([ConstrainedVar], [ConstrainedVar], a)
  -- ^ (negative, positive, a)
runRenderTypeContext a = do
  (s, r) <- runState initialContextState a
  pure (reverse (csNegativeVars s), reverse (csPositiveVars s), r)

runNoContext :: HasErr r => Sem (State ContextState : r) a -> Sem r a
runNoContext a = do
  (s, r) <- runState initialContextState a
  if null (csNegativeVars s <> csPositiveVars s)
    then pure ()
    else throw "Variables were inserted while getting the type with no context"
  pure r

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)
