{-# language QuasiQuotes #-}
module Render.Spec.Extends
  ( structExtends
  ) where

import           Relude                  hiding ( Reader
                                                , Enum
                                                , ask
                                                , Text
                                                )
import           Polysemy
import           Polysemy.Reader
import           Data.Text.Prettyprint.Doc
import           Text.InterpolatedString.Perl6.Unindented

import           Control.Monad.Trans.Cont       ( ContT
                                                , evalContT
                                                )
import           Foreign.Ptr
import           Foreign.Storable
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
structExtends spec = genRe "Extends type family" $ do
  tellExplicitModule (ModName "Graphics.Vulkan.CStruct.Extends")
  typeFamily spec
  classes

typeFamily
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderElem r)
  => Spec
  -> Sem r ()
typeFamily Spec {..} = do
  RenderParams {..} <- ask
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

classes
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderElem r)
  => Sem r ()
classes = do
  tellExport (EData (TyConName "PeekChain"))
  tellExport (EData (TyConName "PokeChain"))
  tellExport (EType (TyConName "Chain"))
  tellImport (TyConName "Extends")
  tellImportWithAll (TyConName "ToCStruct")
  tellImportWithAll (TyConName "FromCStruct")
  tellImport ''Type
  tellImport ''Constraint
  tellImport ''Ptr
  tellImport 'nullPtr
  tellImport 'castPtr
  tellImport 'plusPtr
  tellImport 'evalContT
  tellImport 'lift
  tellImportWithAll ''ContT
  tellImportWith ''Storable 'peek
  tellImportWith ''Storable 'poke
  tellDoc [qqi|
    class ValidExtensions (p :: [Type] -> Type) (cs :: [Type]) where

    instance ValidExtensions p '[] where
    instance (Extends (p '[]) x, ValidExtensions p xs) => ValidExtensions p (x:xs) where

    type family Chain (xs :: [a]) = (r :: a) | r -> xs where
      Chain '[]    = ()
      Chain (x:xs) = (x, Chain xs)

    type family Extendss (p :: Type) (xs :: [Type]) :: Constraint where
      Extendss p '[]      = ()
      Extendss p (x : xs) = (Extends p x, Extendss p xs)

    class PokeChain xs where
      withChain :: Chain xs -> (Ptr (Chain xs) -> IO a) -> IO a

    instance PokeChain '[] where
      withChain () f = f nullPtr

    instance (ToCStruct x, PokeChain xs) => PokeChain (x:xs) where
      withChain (x, xs) f = evalContT $ do
        t <- ContT $ withChain xs
        h <- ContT $ withCStruct x
        lift $ linkChain h t
        lift $ f (castPtr h)

    class PeekChain xs where
      peekChain :: Ptr (Chain xs) -> IO (Chain xs)

    instance PeekChain '[] where
      peekChain _ = pure ()

    instance (FromCStruct x, PeekChain xs) => PeekChain (x:xs) where
      peekChain p = do
        h <- peekCStruct @x (castPtr p)
        tPtr <- peek (p `plusPtr` 8)
        t <- peekChain tPtr
        pure (h, t)

    linkChain :: Ptr a -> Ptr b -> IO ()
    linkChain = undefined -- poke bptr into a
  |]
