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
import           Language.Haskell.TH            ( mkName )

import           Foreign.Ptr
import           Foreign.Storable

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element

renderHandle
  :: (HasErr r, Member (Reader RenderParams) r) => Handle -> Sem r RenderElement
renderHandle Handle {..} = context (unCName hName) $ do
  RenderParams {..} <- ask
  genRe ("handle " <> unCName hName) $ do
    let n = mkTyName hName
    case hDispatchable of
      NonDispatchable -> do
        let t = ConT ''Word64
            c = mkConName hName hName
        tDoc <- renderTypeHighPrec t
        tellDataExport n
        tellImport (TyConName "Zero")
        tellImport ''Storable
        tellDoc
          $   "newtype"
          <+> pretty n
          <+> "="
          <+> pretty c
          <+> tDoc
          <>  line
          <>  indent 2 "deriving newtype (Show, Eq, Ord, Storable, Zero)"
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
        tellExport (EType p)
        tellBoot $ do
          tellDoc $ "data" <+> pretty p
          tellExport (EType p)
        tellInternal (EType p)
        tellDoc $ vsep
          [ "data" <+> pretty p
          , "data"
          <+> pretty n
          <+> "="
          <+> pretty c
          <>  line
          <>  indent
                2
                (vsep
                  [ "{" <+> pretty h <+> "::" <+> tDoc
                  , "," <+> cmdsMemberName <+> "::" <+> cmdsTDoc
                  , "}"
                  ]
                )
          <>  line
          -- TODO: Just compare on ptr
          <>  indent 2 "deriving (Eq, Show)"
          ]
