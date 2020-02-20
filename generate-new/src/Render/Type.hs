{-# language TemplateHaskellQuotes #-}
module Render.Type
  ( Preserve(..)
  , cToHsType
  , namedTy
  )
where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances  ( )
import           Polysemy
import           Polysemy.Reader
import           Foreign.C.Types
import           Foreign.Ptr
import qualified Data.Vector.Storable.Sized    as VSS

import           CType
import           Haskell                       as H
import           Error
import           Render.Element

data Preserve
  = DoNotPreserve
    -- ^ Use more idiomatic haskell types
  | DoPreserve
    -- ^ Use the types from Foreign.C.Types

cToHsType
  :: forall r
   . (HasErr r, MemberWithError (Reader RenderParams) r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsType preserve t = do
  RenderParams {..} <- ask
  t'                <- r
  pure $ case preserve of
    DoNotPreserve -> case mkIdiomaticType t' of
      Nothing -> t'
      Just i  -> itType i
    DoPreserve -> t'

 where
  r :: Sem r H.Type
  r = case t of
    Void -> case preserve of
      DoPreserve    -> pure (TupleT 0)
      DoNotPreserve -> throw "Getting the unpreserved haskell type for void"
    Int    -> pure $ ConT ''CInt
    Float  -> pure $ ConT ''CFloat
    Double -> pure $ ConT ''CDouble
    Char   -> case preserve of
      DoPreserve -> pure $ ConT ''CChar
      DoNotPreserve ->
        throw
          "Getting the unpreserved haskell type for char. This case should be implemented if this char is not better represented by a bytestring"
    Ptr _ Void -> pure $ ConT ''Ptr :@ TupleT 0
    Ptr _ p    -> do
      t <- cToHsType preserve p
      pure $ ConT ''Ptr :@ t
    Array _ (NumericArraySize n) e -> do
      e' <- cToHsType preserve e
      pure $ ConT ''VSS.Vector :@ LitT (NumTyLit (fromIntegral n)) :@ e'
    Array _ (SymbolicArraySize n) e -> do
      RenderParams {..} <- ask
      e'                <- cToHsType preserve e
      pure $ ConT ''VSS.Vector :@ ConT (typeName (mkTyName n)) :@ e'
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
      pure $ ConT . typeName . mkTyName $ n
    Proto ret ps -> do
      retTy <- cToHsType preserve ret
      pTys  <- forV ps $ \(n, c) -> do
        t <- cToHsType preserve c
        pure $ case n of
          Nothing   -> t
          Just name -> namedTy name t
      pure $ foldr (~>) (ConT ''IO :@ retTy) pTys
    c -> throw $ "Unable to get Haskell type for: " <> show c

namedTy :: Text -> H.Type -> H.Type
namedTy name = InfixT (LitT (StrTyLit (toString name))) (typeName ":::")
