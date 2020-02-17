{-# language TemplateHaskellQuotes #-}
module Render.Type
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

data RenderParams = RenderParams
  { mkTyName          :: Text -> Text
  , mkConName         :: Text -> Text
  , mkMemberName      :: Text -> Text
  , mkFunName         :: Text -> Text
  , mkParamName       :: Text -> Text
  , mkPatternName     :: Text -> Text
  , mkHandleName      :: Text -> Text
  , mkFuncPointerName :: Text -> Text
    -- ^ Should be distinct from mkTyName
  }

data Preserve
  = DoNotPreserve
    -- ^ Use more idiomatic haskell types
  | DoPreserve
    -- ^ Use the types from Foreign.C.Types

cToHsType
  :: (HasErr r, MemberWithError (Reader RenderParams) r)
  => Preserve
  -> CType
  -> Sem r H.Type
cToHsType preserve = \case
  Void -> case preserve of
    DoPreserve    -> pure (TupleT 0)
    DoNotPreserve -> throw "Getting the unpreserved haskell type for void"
  Int -> pure $ case preserve of
    DoPreserve    -> ConT ''CInt
    DoNotPreserve -> ConT ''Int32
  Float -> pure $ case preserve of
    DoPreserve    -> ConT ''CFloat
    DoNotPreserve -> ConT ''Float
  Double -> pure $ case preserve of
    DoPreserve    -> ConT ''CDouble
    DoNotPreserve -> ConT ''Double
  Char -> case preserve of
    DoPreserve -> pure $ ConT ''CChar
    DoNotPreserve ->
      throw
        "Getting the unpreserved haskell type for char. This case should be implemented if this char is not better represented by a bytestring"
  Ptr _ Void -> pure $ ConT ''Ptr :@ TupleT 0
  Ptr _ p    -> do
    t <- cToHsType preserve p
    pure $ ConT ''Ptr :@ t
  Array NonConst (NumericArraySize n) e -> do
    e' <- cToHsType preserve e
    pure $ ConT ''VSS.Vector :@ LitT (NumTyLit (fromIntegral n)) :@ e'
  TypeName "uint8_t"  -> pure $ ConT ''Word8
  TypeName "uint16_t" -> pure $ ConT ''Word16
  TypeName "uint32_t" -> pure $ ConT ''Word32
  TypeName "uint64_t" -> pure $ ConT ''Word64
  TypeName "int8_t"   -> pure $ ConT ''Int8
  TypeName "int16_t"  -> pure $ ConT ''Int16
  TypeName "int32_t"  -> pure $ ConT ''Int32
  TypeName "int64_t"  -> pure $ ConT ''Int64
  TypeName "size_t"   -> pure . ConT $ case preserve of
    DoPreserve    -> ''CSize
    DoNotPreserve -> ''Word64
  TypeName n -> do
    RenderParams {..} <- ask
    pure $ ConT . mkName . toString . mkTyName $ n
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
namedTy name = InfixT (LitT (StrTyLit (toString name))) (mkName ":::")
