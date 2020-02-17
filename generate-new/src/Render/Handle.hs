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

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Type

renderHandle
  :: (HasErr r, Member (Reader RenderParams) r) => Handle -> Sem r RenderElement
renderHandle Handle {..} = context hName $ do
  RenderParams {..} <- ask
  genRe ("handle " <> hName) $ do
    let n = mkHandleName hName
        p = n <> "_T"
        t = ConT ''Ptr :@ ConT (mkName (toString p))
    tDoc <- renderType t
    tellExport (EType n)
    tellInternal (EType p)
    tellDoc $ vsep ["data" <+> pretty p, "type" <+> pretty n <+> "=" <+> tDoc]
