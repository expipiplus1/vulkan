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
          <+> tDoc
          <>  line
          <>  indent 2 "deriving newtype (Show, Eq, Ord, Storable, Zero)"
      Dispatchable -> do
        let p = mkEmptyDataName n
            c = mkConName n n
            h = mkDispatchableHandlePtrName n
            t = ConT ''Ptr :@ ConT (typeName p)
        (cmdsMemberName, cmdsMemberTy) <- case hLevel of
          NoHandleLevel -> throw "Dispatchable handle without a level"
          Instance      -> pure ("instanceCmds", ConT (typeName "InstanceCmds"))
          Device        -> pure ("deviceCmds", ConT (typeName "DeviceCmds"))
        tDoc     <- renderType t
        cmdsTDoc <- renderType cmdsMemberTy
        tellExport (EData n)
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
