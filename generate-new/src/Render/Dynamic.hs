{-# language TemplateHaskellQuotes #-}
{-# language QuasiQuotes #-}
module Render.Dynamic
  where

import           Relude                  hiding ( Type
                                                , State
                                                )
import           Data.Text.Prettyprint.Doc
import           Text.InterpolatedString.Perl6.Unindented
import qualified Data.List.Extra               as List
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Data.Vector.Extra              ( Vector
                                                , pattern (:<|)
                                                )
import qualified Data.Vector                   as V

import           Foreign.Ptr
import           GHC.Ptr

import           Spec.Parse
import           Haskell                       as H
import           Haskell.Name                   ( )
import           Marshal
import           Error
import           Render.Element.Write
import           Render.Element
import           Render.Type
import           Render.SpecInfo
import           Render.Utils
import qualified CType                         as C

renderDynamicLoader
  :: (HasErr r, HasRenderParams r, HasTypeInfo r, HasSpecInfo r)
  => Vector MarshaledCommand
  -> Sem r RenderElement
renderDynamicLoader cs = do
  RenderParams {..} <- input
  genRe "dynamic loader" $ do
    tellExplicitModule (ModName "Graphics.Vulkan.Dynamic")
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
      ty <- cToHsTypeQuantified DoLower $ commandType c
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
  let getInstanceProcAddr' = mkFunName "vkGetInstanceProcAddr'"
  (binds, apps) <- initCmdsStmts (pretty getInstanceProcAddr') instanceCommands
  tellExport (ETerm n)
  tellDoc [qqi|
    {n} :: {tDoc}
    {n} handle = do
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
    c@Command {..} <-
      maybe (throw "Unable to find vkGetDeviceProcAddr command") pure
        =<< getCommand "vkGetDeviceProcAddr"
    renderTypeHighPrecSource =<< cToHsTypeWithHoles DoLower (commandType c)
  (binds, apps) <- initCmdsStmts "getDeviceProcAddr'" deviceCommands
  tellExport (ETerm n)
  let getInstanceProcAddr' = mkFunName "vkGetInstanceProcAddr'"
  tellDoc [qqi|
    {n} :: {tDoc}
    {n} instanceCmds handle = do
      pGetDeviceProcAddr <- castFunPtr @_ @{getDeviceProcAddrTDoc}
          <$> {getInstanceProcAddr'} (instanceCmdsHandle instanceCmds) (GHC.Ptr.Ptr "vkGetDeviceProcAddr"#)
      let getDeviceProcAddr' = mkVkGetDeviceProcAddr pGetDeviceProcAddr
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
  -> Vector MarshaledCommand
  -> Sem r ([Doc ()], [Doc ()])
  -- ^ binds and cast pointers
initCmdsStmts getProcAddr commands = do
  tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
  let binds = commands <&> \MarshaledCommand { mcCommand = Command {..} } ->
        pretty (unCName cName)
          <+> "<-"
          <+> getProcAddr
          <+> "handle (Ptr \""
          <>  pretty (unCName cName)
          <>  "\"#)"
  apps <- forV commands $ \MarshaledCommand { mcCommand = c@Command {..} } -> do
    fTyDoc <- renderTypeHighPrecSource
      =<< cToHsTypeWithHoles DoLower (commandType c)
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
  c@Command {..}    <-
    maybe (throw "Unable to find vkGetInstanceProcAddr command") pure
      =<< getCommand "vkGetInstanceProcAddr"
  ty   <- cToHsTypeQuantified DoLower (commandType c)
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
  RenderParams {..} <- input
  c@Command {..}    <-
    maybe (throw "Unable to find vkGetDeviceProcAddr command") pure
      =<< getCommand "vkGetDeviceProcAddr"
  ty   <- cToHsTypeQuantified DoLower (commandType c)
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

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
