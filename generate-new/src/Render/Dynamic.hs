{-# language TemplateHaskellQuotes #-}
{-# language QuasiQuotes #-}
module Render.Dynamic
  ( renderDynamicLoader
  ) where

import qualified Data.List.Extra               as List
import           Prettyprinter
import qualified Data.Vector                   as V
import           Data.Vector.Extra              ( pattern (:<|)
                                                , Vector
                                                )
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Relude                  hiding ( State
                                                , Type
                                                )
import           Text.InterpolatedString.Perl6.Unindented

import           Foreign.Ptr
import           GHC.Ptr

import qualified CType                         as C
import           Control.Exception              ( throwIO )
import           Error
import           Foreign.Marshal                ( alloca )
import           Foreign.Storable               ( Storable )
import           Haskell                       as H
import           Haskell.Name                   ( )
import           Marshal
import           Render.Element
import           Render.Element.Write
import           Render.SpecInfo
import           Render.Type
import           Render.Utils
import           Spec.Parse

renderDynamicLoader
  :: (HasErr r, HasRenderParams r, HasTypeInfo r, HasSpecInfo r)
  => SpecFlavor
  -> Vector MarshaledCommand
  -> Sem r RenderElement
renderDynamicLoader t cs = do
  RenderParams {..} <- input
  genRe "dynamic loader" $ do
    tellExplicitModule =<< mkModuleName ["Dynamic"]
    tellLanguageExtension (LanguageExtension "NoDuplicateRecordFields")
    tellNotReexportable
    enabledCommands <- V.filterM
      (fmap isNothing . getDisabledCommand . cName . mcCommand)
      cs
    deviceCommands <- V.filterM (fmap (== Device) . getCommandLevel)
                                enabledCommands
    instanceCommands <- V.filterM (fmap (== Instance) . getCommandLevel)
                                  enabledCommands

    unless (null instanceCommands) $ do
      loader
        "Instance"
        (ConT ''Ptr :@ ConT
          (typeName (mkEmptyDataName (CName $ camelPrefix <> "Instance")))
        )
        instanceCommands
      writeGetInstanceProcAddr t
      writeInitInstanceCmds instanceCommands

    unless (null deviceCommands) $ do
      loader
        "device"
        (ConT ''Ptr :@ ConT
          (typeName (mkEmptyDataName (CName $ camelPrefix <> "Device")))
        )
        deviceCommands
      writeMkGetDeviceProcAddr
      writeInitDeviceCmds deviceCommands

getCommandLevel :: HasSpecInfo r => MarshaledCommand -> Sem r HandleLevel
getCommandLevel MarshaledCommand { mcCommand = Command {..} } =
  case cParameters of
    Parameter { pType = C.TypeName n } :<| _ -> getHandle n >>= \case
      Just h  -> pure $ hLevel h
      Nothing -> pure NoHandleLevel
    _ -> pure NoHandleLevel

loader
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => Text
  -> Type
  -> Vector MarshaledCommand
  -> Sem r ()
loader level handleType commands = do
  RenderParams {..} <- input
  memberDocs        <-
    forV commands $ \MarshaledCommand { mcCommand = c@Command {..} } -> do
      ty <- cToHsTypeWrapped DoLower $ commandType c
      let pTy = applyInsideForall (ConT ''FunPtr) ty
      tDoc <- renderTypeSource pTy
      let memberName = mkFuncPointerMemberName cName
      pure $ pretty memberName <+> "::" <+> tDoc
  let n                = CName $ level <> "Cmds"
      tyName           = mkTyName n
      conName          = mkConName n n
      handleMemberName = mkDispatchableHandlePtrName n
  handleTDoc <- renderTypeSource handleType
  let handleDoc = pretty handleMemberName <+> "::" <+> handleTDoc
  tellDataExport tyName
  tellImportWithAll (TyConName "Zero")
  tellImport 'nullPtr
  tellImport 'nullFunPtr
  tellDoc $ vsep
    [ "data"
    <+> pretty tyName
    <+> "="
    <+> pretty conName
    <>  line
    <>  indent 2 (braceList (handleDoc : V.toList memberDocs))
    <>  line
    , "deriving instance Eq" <+> pretty tyName
    , "deriving instance Show" <+> pretty tyName
    , "instance Zero" <+> pretty tyName <+> "where" <> line <> indent
      2
      ("zero =" <+> pretty conName <> line <> indent
        2
        (vsep . fmap sep $ List.chunksOf
          8
          ("nullPtr" : ("nullFunPtr" <$ V.toList memberDocs))
        )
      )
    ]

----------------------------------------------------------------
-- Filling out the structures
----------------------------------------------------------------

writeInitInstanceCmds
  :: ( HasTypeInfo r
     , HasErr r
     , HasRenderElem r
     , HasRenderParams r
     , HasSpecInfo r
     )
  => Vector MarshaledCommand
  -> Sem r ()
writeInitInstanceCmds instanceCommands = do
  RenderParams {..} <- input
  let n = mkFunName "initInstanceCmds"
  tDoc <- renderTypeSource
    (  (ConT ''Ptr :@ ConT
         (typeName (mkEmptyDataName (CName $ camelPrefix <> "Instance")))
       )
    ~> (ConT ''IO :@ ConT (typeName (mkTyName "InstanceCmds")))
    )
  tellImport 'castFunPtr
  tellImport 'nullFunPtr
  let getInstanceProcAddr' =
        mkFunName (CName $ lowerPrefix <> "GetInstanceProcAddr'")
      getFirstInstanceProcAddr = "getFirstInstanceProcAddr" :: Text
  (binds, apps) <- initCmdsStmts (pretty getFirstInstanceProcAddr)
                                 (pretty getInstanceProcAddr')
                                 instanceCommands
  tellExport (ETerm n)
  tellDoc [qqi|
    {n} :: {tDoc}
    {n} handle = do
      let {getFirstInstanceProcAddr} = \\case
            []   -> pure nullFunPtr
            x:xs -> do
              p <- {getInstanceProcAddr'} handle x
              if p /= nullFunPtr
                then pure p
                else {getFirstInstanceProcAddr} xs
    {indent 2 $ vsep binds}
      pure $ InstanceCmds handle
    {indent 4 $ vsep apps}
  |]

writeInitDeviceCmds
  :: ( HasTypeInfo r
     , HasSpecInfo r
     , HasErr r
     , MemberWithError (State RenderElement) r
     , HasRenderParams r
     )
  => Vector MarshaledCommand
  -> Sem r ()
writeInitDeviceCmds deviceCommands = do
  RenderParams {..} <- input
  let n = mkFunName "initDeviceCmds"
  tDoc <- renderTypeSource
    (  ConT (typeName (mkTyName "InstanceCmds"))
    ~> (ConT ''Ptr :@ ConT
         (typeName (mkEmptyDataName (CName $ camelPrefix <> "Device")))
       )
    ~> (ConT ''IO :@ ConT (typeName (mkTyName "DeviceCmds")))
    )
  tellImport 'castFunPtr
  tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
  getDeviceProcAddrTDoc <- do
    c <- maybe (throw "Unable to find GetDeviceProcAddr command") pure
      =<< getCommand (CName $ lowerPrefix <> "GetDeviceProcAddr")
    renderTypeHighPrecSource =<< cToHsTypeWrapped DoLower (commandType c)
  let getFirstDeviceProcAddr = "getFirstDeviceProcAddr" :: Text
  (binds, apps) <- initCmdsStmts (pretty getFirstDeviceProcAddr)
                                 "getDeviceProcAddr'"
                                 deviceCommands
  tellExport (ETerm n)
  let getInstanceProcAddr' =
        mkFunName (CName $ lowerPrefix <> "GetInstanceProcAddr'")
  tellDoc [qqi|
    {n} :: {tDoc}
    {n} instanceCmds handle = do
      pGetDeviceProcAddr <- castFunPtr @_ @{getDeviceProcAddrTDoc}
          <$> {getInstanceProcAddr'} (instanceCmdsHandle instanceCmds) (GHC.Ptr.Ptr "{lowerPrefix}GetDeviceProcAddr"#)
      let getDeviceProcAddr' = mk{camelPrefix}GetDeviceProcAddr pGetDeviceProcAddr
          {getFirstDeviceProcAddr} = \\case
            []   -> pure nullFunPtr
            x:xs -> do
              p <- getDeviceProcAddr' handle x
              if p /= nullFunPtr
                then pure p
                else {getFirstDeviceProcAddr} xs
    {indent 2 $ vsep binds}
      pure $ DeviceCmds handle
    {indent 4 $ vsep apps}
  |]

initCmdsStmts
  :: ( HasTypeInfo r
     , HasSpecInfo r
     , HasErr r
     , MemberWithError (State RenderElement) r
     , HasRenderParams r
     )
  => Doc ()
  -> Doc ()
  -> Vector MarshaledCommand
  -> Sem r ([Doc ()], [Doc ()])
  -- ^ binds and cast pointers
initCmdsStmts getFirstProcAddr getProcAddr commands = do
  SpecInfo {..} <- input
  tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
  let binds = commands <&> \MarshaledCommand { mcCommand = Command {..} } ->
        let otherNames = siGetAliases cName
            nameString name =
              parens $ "Ptr \"" <> pretty (unCName name) <> "\"#"
            nameStrings names = list (nameString <$> names)
        in  case otherNames of
              [] ->
                pretty (unCName cName)
                  <+> "<-"
                  <+> getProcAddr
                  <+> "handle"
                  <+> nameString cName
              ns ->
                pretty (unCName cName)
                  <+> "<-"
                  <+> getFirstProcAddr
                  <+> nameStrings (ns <> [cName])
  apps <- forV commands $ \MarshaledCommand { mcCommand = c@Command {..} } -> do
    fTyDoc <- renderTypeHighPrecSource
      =<< cToHsTypeWrapped DoLower (commandType c)
    pure $ parens ("castFunPtr @_ @" <> fTyDoc <+> pretty (unCName cName))
  pure (toList binds, toList apps)

----------------------------------------------------------------
-- Bootstrapping commands
----------------------------------------------------------------

writeGetInstanceProcAddr
  :: ( HasTypeInfo r
     , HasSpecInfo r
     , HasErr r
     , MemberWithError (State RenderElement) r
     , HasRenderParams r
     )
  => SpecFlavor
  -> Sem r ()
writeGetInstanceProcAddr t = do
  RenderParams {..} <- input
  c <- maybe (throw "Unable to find GetInstanceProcAddr command") pure
    =<< getCommand (CName $ lowerPrefix <> "GetInstanceProcAddr")
  ty   <- cToHsTypeWrapped DoLower (commandType c)
  tDoc <- renderTypeSource ty
  let n = mkFunName (CName $ lowerPrefix <> "GetInstanceProcAddr'")
  tellExport (ETerm n)
  case t of
    SpecVk -> tellDoc [qqi|
        -- | A version of '{mkFunName (CName $ lowerPrefix <> "GetInstanceProcAddr")}' which can be called
        -- with a null pointer for the instance.
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "{lowerPrefix}GetInstanceProcAddr" {n} :: {tDoc}
      |]
    SpecXr -> do
      tellImportWithAll ''Storable
      tellImportWithAll (mkTyName "XrResult")
      tellImport 'alloca
      tellImport 'throwIO
      let succeeded   = mkPatternName "XR_SUCCEEDED"
          unsupported = mkPatternName "XR_ERROR_FUNCTION_UNSUPPORTED"
          failed      = mkPatternName "XR_FAILED"
      tellImport succeeded
      tellImport unsupported
      tellImport failed
      tellImportWithAll exceptionTypeName
      tellDoc [qqi|
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "{lowerPrefix}GetInstanceProcAddr" {n}' :: {tDoc}

        -- | A version of '{mkFunName (CName $ lowerPrefix <> "GetInstanceProcAddr")}' which can be called
        -- with a null pointer for the instance.
        {n}
          :: Ptr Instance_T -> ("name" ::: Ptr CChar) -> IO PFN_xrVoidFunction
        {n} inst name = do
          alloca $ \\r -> {n}' inst name r >>= \\case
            {succeeded}                  -> peek r
            {unsupported}                -> pure nullFunPtr
            e@{failed}                   -> throwIO ({exceptionTypeName} e)

      |]

writeMkGetDeviceProcAddr
  :: ( HasTypeInfo r
     , HasSpecInfo r
     , HasErr r
     , MemberWithError (State RenderElement) r
     , HasRenderParams r
     )
  => Sem r ()
writeMkGetDeviceProcAddr = do
  RenderParams {..} <- input
  c <- maybe (throw "Unable to find GetDeviceProcAddr command") pure
    =<< getCommand (CName $ lowerPrefix <> "GetDeviceProcAddr")
  ty   <- cToHsTypeWrapped DoLower (commandType c)
  tDoc <- renderTypeSource (ConT ''FunPtr :@ ty ~> ty)
  tellDoc [qqi|
    foreign import ccall
    #if !defined(SAFE_FOREIGN_CALLS)
      unsafe
    #endif
      "dynamic" mk{camelPrefix}GetDeviceProcAddr
      :: {tDoc}
  |]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

commandType :: Command -> C.CType
commandType Command {..} = C.Proto
  cReturnType
  [ (Just (unCName pName), pType) | Parameter {..} <- V.toList cParameters ]

applyInsideForall :: Type -> Type -> Type
applyInsideForall f x = case x of
  ForallT vs ctx t -> ForallT vs ctx (f :@ t)
  _                -> f :@ x
