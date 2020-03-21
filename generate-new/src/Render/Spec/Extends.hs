module Render.Spec.Extends
  where

import           Relude                  hiding ( Reader
                                                , Enum
                                                , ask
                                                , Text
                                                )
import           Polysemy
import           Polysemy.Reader
import           Data.Text.Prettyprint.Doc

import           GHC.TypeLits

import           Error
import           Render.Element
import           Spec.Types
import           Render.SpecInfo
import           Haskell                        ( renderTypeHighPrecSource )
import           Haskell.Name
import           Render.Type
import           CType

structExtends
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Spec
  -> Sem r RenderElement
structExtends Spec {..} = genRe "Extends type family" $ do
  RenderParams {..} <- ask
  tellExplicitModule (ModName "Graphics.Vulkan.CStruct.Extends")
  tellExport (EType (TyConName "Extends"))
  tellImport ''Type
  tellImport ''TypeError
  tellImport ''Constraint
  tellImportWithAll ''ErrorMessage
  cases <-
    fmap (fmap snd . sortOn fst . concat)
    $ forV (toList specStructs)
    $ \child -> do
        cTyDoc <- renderTypeHighPrecSource
          =<< cToHsType DoNotPreserve (TypeName (sName child))
        forV (toList (sExtends child)) $ \parent -> do
          pTyDoc <- renderTypeHighPrecSource
            =<< cToHsType DoNotPreserve (TypeName parent)
          pure (parent, "Extends" <+> pTyDoc <+> cTyDoc <+> "= ()")
  tellDoc
    $  "type family Extends (a :: Type) (b :: Type) :: Constraint where"
    <> line
    <> indent
         2
         (vsep
           (cases
           <> [ "Extends a b = TypeError (ShowType a :<>: Text \" is not extended by \" :<>: ShowType b)"
              ]
           )
         )
