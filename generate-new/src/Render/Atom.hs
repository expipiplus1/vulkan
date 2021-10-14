module Render.Atom where

import           Prettyprinter
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Handle
                                                , lift
                                                )

import           Foreign.Storable
import           Numeric
import           Text.Show

import           Error
import           Haskell                       as H
import           Render.Element
import           Spec.Parse

renderAtom :: (HasErr r, HasRenderParams r) => Atom -> Sem r RenderElement
renderAtom Atom {..} = context (unCName atName) $ do
  RenderParams {..} <- input
  genRe ("atom " <> unCName atName) $ do
    let n = mkTyName atName
    let t = ConT ''Word64
        c = mkConName atName atName
    tDoc <- renderTypeHighPrec t
    tellDataExport n
    tellImport (TyConName "Zero")
    tellImport ''Storable
    tellImport 'showHex
    tellImport 'showParen
    tellDocWithHaddock $ \getDoc ->
      vsep
        $  [ getDoc (TopLevel atName)
           , "newtype" <+> pretty n <+> "=" <+> pretty c <+> tDoc
           , indent 2 "deriving newtype (Eq, Ord, Storable, Zero)"
           ]
        <> [ "instance Show" <+> pretty n <+> "where" <> line <> indent
               2
               (   "showsPrec p"
               <+> parens (pretty c <+> "x")
               <+> "= showParen (p >= 11)"
               <+> parens
                     (   "showString"
                     <+> viaShow (unName c <> " 0x")
                     <+> ". showHex x"
                     )
               )
           ]
