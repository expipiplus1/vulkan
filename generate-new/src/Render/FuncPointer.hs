{-# language TemplateHaskellQuotes #-}
module Render.FuncPointer
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Data.Text.Prettyprint.Doc
import           Language.Haskell.TH.Syntax
import           Polysemy
import           Polysemy.Reader
import           Foreign.Ptr

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Type
import           CType

renderFuncPointer
  :: (HasErr r, Member (Reader RenderParams) r)
  => FuncPointer
  -> Sem r RenderElement
renderFuncPointer FuncPointer {..} = contextShow fpName $ do
  RenderParams {..} <- ask
  genRe ("func pointer " <> fpName) $ do
    let p = mkTyName fpName
        n = mkFuncPointerName fpName
    tDoc    <- renderType =<< cToHsType DoPreserve =<< stripPtr fpType
    tPtrDoc <- renderType (ConT ''FunPtr :@ ConT (mkName (toString n)))
    tellExport (EType p)
    tellExport (EType n)
    tellDoc
      $   "type" <+> pretty n <+> "=" <+> tDoc
      <>  line
      <>  "type" <+> pretty p <+> "=" <+> tPtrDoc

stripPtr :: HasErr r => CType -> Sem r CType
stripPtr = \case
  Ptr _ t -> pure t
  _ -> throw "trying to strip the pointer from a non-pointer type"

