{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Handle
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , Handle
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader

import           Foreign.Ptr
import           Foreign.Storable

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element

renderHandle
  :: (HasErr r, Member (Reader RenderParams) r) => Handle -> Sem r RenderElement
renderHandle Handle {..} = context hName $ do
  RenderParams {..} <- ask
  genRe ("handle " <> hName) $ do
    let n = mkHandleName hName
    case hDispatchable of
      NonDispatchable -> do
        let t = ConT ''Word64
            c = mkConName hName hName
        tDoc <- renderTypeHighPrec t
        tellExport (EData n)
        tellImport (TyConName "Zero")
        tellImport ''Storable
        tellDoc
          $   "newtype"
          <+> pretty n
          <+> "="
          <+> pretty c
          <+> tDoc <> line
          <>  indent 2 "deriving newtype (Eq, Ord, Storable, Zero)"
      Dispatchable -> do
        let p = n <> "_T"
            t = ConT ''Ptr :@ ConT (typeName p)
        tDoc <- renderType t
        tellExport (EType n)
        tellInternal (EType p)
        tellDoc
          $ vsep ["data" <+> pretty p, "type" <+> pretty n <+> "=" <+> tDoc]
