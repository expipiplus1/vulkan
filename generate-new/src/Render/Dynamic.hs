{-# language TemplateHaskellQuotes #-}
{-# language QuasiQuotes #-}
module Render.Dynamic
  where

import qualified Data.List.Extra               as List
import           Data.Text.Prettyprint.Doc
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
import           Error
import           Haskell                       as H
import           Haskell.Name                   ( )
import           Marshal
import           Render.Element
import           Render.Element.Write
import           Render.SpecInfo
import           Render.Type
import           Render.Utils
import           Spec.Parse
import           VkModulePrefix

renderDynamicLoader
  :: (HasErr r, HasRenderParams r, HasTypeInfo r, HasSpecInfo r)
  => Vector MarshaledCommand
  -> Sem r RenderElement
renderDynamicLoader cs = do
  RenderParams {..} <- input
  genRe "dynamic loader" $ do
    tellExplicitModule (vulkanModule ["Dynamic"])
    tellLanguageExtension (LanguageExtension "NoDuplicateRecordFields")
    tellNotReexportable
    enabledCommands <- V.filterM
      (fmap isNothing . getDisabledCommand . cName . mcCommand)
      cs
    deviceCommands <- V.filterM (fmap (== Device) . getCommandLevel)
                                enabledCommands
    instanceCommands <- V.filterM (fmap (== Instance) . getCommandLevel)
                                  enabledCommands
    loader "Instance"
           (ConT ''Ptr :@ ConT (typeName (mkEmptyDataName "VkInstance")))
           instanceCommands
    writeGetInstanceProcAddr
    writeInitInstanceCmds instanceCommands
    loader "device"
           (ConT ''Ptr :@ ConT (typeName (mkEmptyDataName "VkDevice")))
           deviceCommands
    writeMkGetDeviceProcAddr
    writeInitDeviceCmds deviceCommands

getCommandLevel
  :: HasSpecInfo r => MarshaledCommand -> Sem r HandleLevel
getCommandLevel MarshaledCommand { mcCommand = Command {..} } =
  case cParameters of
    Parameter { pType = C.TypeName n } :<| _ ->
      getHandle n >>= \case
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
    (  (ConT ''Ptr :@ ConT (typeName (mkEmptyDataName "VkInstance")))
    ~> (ConT ''IO :@ ConT (typeName (mkTyName "InstanceCmds")))
    )
  tellImport 'castFunPtr
  tellImport 'nullFunPtr
  let getInstanceProcAddr'     = mkFunName "vkGetInstanceProcAddr'"
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
    ~> (ConT ''Ptr :@ ConT (typeName (mkEmptyDataName "VkDevice")))
    ~> (ConT ''IO :@ ConT (typeName (mkTyName "DeviceCmds")))
    )
  tellImport 'castFunPtr
  tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
  getDeviceProcAddrTDoc <- do
    c <- maybe (throw "Unable to find vkGetDeviceProcAddr command") pure
      =<< getCommand "vkGetDeviceProcAddr"
    renderTypeHighPrecSource =<< cToHsTypeWrapped DoLower (commandType c)
  let getFirstDeviceProcAddr = "getFirstDeviceProcAddr" :: Text
  (binds, apps) <- initCmdsStmts (pretty getFirstDeviceProcAddr)
                                 "getDeviceProcAddr'"
                                 deviceCommands
  tellExport (ETerm n)
  let getInstanceProcAddr' = mkFunName "vkGetInstanceProcAddr'"
  tellDoc [qqi|
{n} :: {tDoc}
{n} instanceCmds handle = do
  pGetDeviceProcAddr <- castFunPtr @_ @{getDeviceProcAddrTDoc}
      <$> {getInstanceProcAddr'} (instanceCmdsHandle instanceCmds) (GHC.Ptr.Ptr "vkGetDeviceProcAddr"#)
  let getDeviceProcAddr' = mkVkGetDeviceProcAddr pGetDeviceProcAddr
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
            nameString name = parens $ "Ptr \"" <> pretty (unCName name) <> "\"#"
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
  => Sem r ()
writeGetInstanceProcAddr = do
  RenderParams {..} <- input
  c <- maybe (throw "Unable to find vkGetInstanceProcAddr command") pure
    =<< getCommand "vkGetInstanceProcAddr"
  ty   <- cToHsTypeWrapped DoLower (commandType c)
  tDoc <- renderTypeSource ty
  let n = mkFunName "vkGetInstanceProcAddr'"
  tellExport (ETerm n)
  tellDoc [qqi|
-- | A version of '{mkFunName "vkGetInstanceProcAddr"}' which can be called
-- with a null pointer for the instance.
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetInstanceProcAddr" {n} :: {tDoc}
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
  c <- maybe (throw "Unable to find vkGetDeviceProcAddr command") pure
    =<< getCommand "vkGetDeviceProcAddr"
  ty   <- cToHsTypeWrapped DoLower (commandType c)
  tDoc <- renderTypeSource (ConT ''FunPtr :@ ty ~> ty)
  tellDoc [qqi|
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceProcAddr
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
