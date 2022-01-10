module Render.Handle where

import           Prettyprinter
import           Language.Haskell.TH            ( mkName )
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Handle
                                                , lift
                                                )

import           Foreign.Ptr
import           Foreign.Storable
import           Numeric
import           Text.Show

import           Error
import           Haskell                       as H
import           Render.Element
import           Spec.Parse

renderHandle :: (HasErr r, HasRenderParams r) => Handle -> Sem r RenderElement
renderHandle Handle {..} = context (unCName hName) $ do
  RenderParams {..} <- input
  genRe ("handle " <> unCName hName) $ do
    let n = mkTyName hName
    case hDispatchable of
      NonDispatchable -> do
        let t = ConT ''Word64
            c = mkConName hName hName
        tDoc <- renderTypeHighPrec t
        tellDataExport n
        tellImport (TyConName "Zero")
        tellImport (TyConName "IsHandle")
        tellImport ''Storable
        tellImport 'showHex
        tellImport 'showParen
        hasObjectType <- forV (objectTypePattern hName) $ \objectType -> do
          tellImport objectType
          tellImportWithAll (TyConName "HasObjectType")
          pure
            $   "instance HasObjectType"
            <+> pretty n
            <+> "where"
            <>  line
            <>  indent
                  2
                  ("objectTypeAndHandle (" <> pretty c <+> "h) =" <+> tupled
                    [pretty objectType, "h"]
                  )
        tellDocWithHaddock $ \getDoc ->
          vsep
            $  [ getDoc (TopLevel hName)
               , "newtype" <+> pretty n <+> "=" <+> pretty c <+> tDoc
               , indent 2 "deriving newtype (Eq, Ord, Storable, Zero)"
               , indent 2 "deriving anyclass (IsHandle)"
               ]
            <> toList hasObjectType
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
      Dispatchable -> do
        let p = mkEmptyDataName hName
            c = mkConName hName hName
            h = mkDispatchableHandlePtrName hName
            t = ConT ''Ptr :@ ConT (typeName p)
        (cmdsMemberName, cmdsMemberTy) <- case hLevel of
          NoHandleLevel -> throw "Dispatchable handle without a level"
          Instance      -> pure ("instanceCmds", ConT (mkName "InstanceCmds"))
          Device        -> pure ("deviceCmds", ConT (mkName "DeviceCmds"))
        tDoc     <- renderType t
        cmdsTDoc <- renderType cmdsMemberTy
        tellDataExport n
        tellExport (EType p) { exportReexportable = NotReexportable }
        tellBoot $ do
          tellDoc $ "data" <+> pretty p
          tellExport (EType p)
        tellInternal (EType p)
        tellImportWithAll (TyConName "Zero")
        tellImport (TyConName "IsHandle")
        hasObjectType <- forV (objectTypePattern hName) $ \objectType -> do
          tellImport objectType
          tellImport 'WordPtr
          tellImport 'ptrToWordPtr
          tellImportWithAll (TyConName "HasObjectType")
          pure
            $   "instance HasObjectType"
            <+> pretty n
            <+> "where"
            <>  line
            <>  indent
                  2
                  (   "objectTypeAndHandle"
                  <+> parens
                        (   pretty c
                        <+> parens "ptrToWordPtr -> WordPtr h"
                        <+> "_"
                        )
                  <+> "="
                  <+> tupled [pretty objectType, "fromIntegral h"]
                  )
        tellDocWithHaddock $ \getDoc ->
          vsep
            $  [ "-- | An opaque type for representing pointers to"
               <+> pretty (unCName hName)
               <+> "handles"
               , "data" <+> pretty p
               , getDoc (TopLevel hName)
               , "data" <+> pretty n <+> "=" <+> pretty c
               , indent
                 2
                 (vsep
                   [ "{" <+> pretty h <+> "::" <+> tDoc
                   , "," <+> cmdsMemberName <+> "::" <+> cmdsTDoc
                   , "}"
                   ]
                 )
          -- TODO: Just compare on ptr
               , indent 2 "deriving stock (Eq, Show)"
               , indent 2 "deriving anyclass (IsHandle)"
               , "instance Zero" <+> pretty n <+> "where" <> line <> indent
                 2
                 ("zero =" <+> pretty c <+> "zero zero")
               ]
            <> toList hasObjectType
