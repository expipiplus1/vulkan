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
import           Foreign.C.Types
import           Foreign.Ptr

import           CType
import           Haskell                       as H
import           Error

data RenderParams = RenderParams
  { mkTyName      :: Text -> Text
  , mkConName     :: Text -> Text
  , mkMemberName  :: Text -> Text
  , mkFunName     :: Text -> Text
  , mkParamName   :: Text -> Text
  , mkPatternName :: Text -> Text
  }

data Preserve = DoNotPreserve | DoPreserve

cToHsType :: HasErr r => Preserve -> CType -> Sem r H.Type
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
  Ptr _ Void -> pure $ ConT ''Ptr :@ TupleT 0
  Ptr _ p    -> do
    t <- cToHsType preserve p
    pure $ ConT ''Ptr :@ t
  TypeName n -> pure $ ConT . mkName . toString $ n
  c          -> throw $ "Unable to get Haskell type for: " <> show c
