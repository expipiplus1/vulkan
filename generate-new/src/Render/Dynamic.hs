{-# language TemplateHaskellQuotes #-}
{-# language QuasiQuotes #-}
module Render.Dynamic
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , State
                                                , Type
                                                )
import           Data.Text.Prettyprint.Doc
import           Text.InterpolatedString.Perl6.Unindented
import           Polysemy
import           Polysemy.Reader
import           Polysemy.State
import           Data.Vector.Extra              ( Vector
                                                , pattern (:<|)
                                                )
import qualified Data.Vector                   as V

import           Foreign.Ptr
import           GHC.Ptr

import           Spec.Parse
import           Haskell                       as H
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
  RenderParams {..} <- ask
  genRe "dynamic loader" $ do
    tellExplicitModule (ModName "Graphics.Vulkan.Dynamic")
    deviceCommands   <- V.filterM (fmap (== Device) . getCommandLevel) cs
    instanceCommands <- V.filterM (fmap (== Instance) . getCommandLevel) cs
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
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSpecInfo r
     )
  => Text
  -> Type
  -> Vector MarshaledCommand
  -> Sem r ()
loader level handleType commands = do
  RenderParams {..} <- ask
  memberDocs        <-
    forV commands $ \MarshaledCommand { mcCommand = Command {..} } -> do
      ty <- cToHsType
        DoLower
        (C.Proto
          cReturnType
          [ (Just pName, pType) | Parameter {..} <- V.toList cParameters ]
        )
      let pTy = ConT ''FunPtr :@ ty
      tDoc <- renderTypeSource pTy
      let memberName = mkFuncPointerMemberName cName
      pure $ pretty memberName <+> "::" <+> tDoc
  let n                = level <> "Cmds"
      tyName           = mkTyName n
      conName          = mkConName n n
      handleMemberName = mkMemberName (n <> "Handle")
  handleTDoc <- renderTypeSource handleType
  let handleDoc = pretty handleMemberName <+> "::" <+> handleTDoc
  tellDataExport tyName
  tellDoc
    $   "data"
    <+> pretty tyName
    <+> "="
    <+> pretty conName
    <>  line
    <>  indent 2 (braceList (handleDoc : V.toList memberDocs))
    <>  line
    <>  indent 2 ("deriving (Eq, Show)")

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
  RenderParams {..} <- ask
  let n = mkFunName "initInstanceCmds"
  tDoc <- renderTypeSource
    (  (ConT ''Ptr :@ ConT (typeName (mkEmptyDataName "VkInstance")))
    ~> (ConT ''IO :@ ConT (typeName (mkTyName "InstanceCmds")))
    )
  tellImport 'castFunPtr
  apps <-
    fmap (indent 2 . appList)
    . forV instanceCommands
    $ \MarshaledCommand { mcCommand = Command {..} } -> do
        fTyDoc <- renderTypeHighPrecSource =<< cToHsType
          DoLower
          (C.Proto
            cReturnType
            [ (Just pName, pType) | Parameter {..} <- V.toList cParameters ]
          )
        tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
        pure
          $ parens
              [qqi|castFunPtr @_ @{fTyDoc} <$> vkGetInstanceProcAddr' handle (Ptr "{cName}\\NUL"#)|]
  tellExport (ETerm n)
  tellDoc [qqi|
    {n} :: {tDoc}
    {n} handle = InstanceCmds handle
    {apps}
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
  RenderParams {..} <- ask
  let n = mkFunName "initDeviceCmds"
  tDoc <- renderTypeSource
    (  ConT (typeName (mkTyName "InstanceCmds"))
    ~> (ConT ''Ptr :@ ConT (typeName (mkEmptyDataName "VkDevice")))
    ~> (ConT ''IO :@ ConT (typeName (mkTyName "DeviceCmds")))
    )
  tellImport 'castFunPtr
  tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
  getDeviceProcAddrTDoc <- do
    Command {..} <-
      maybe (throw "Unable to find vkGetDeviceProcAddr command") pure
        =<< getCommand "vkGetDeviceProcAddr"
    renderTypeHighPrecSource =<< cToHsType
      DoLower
      (C.Proto
        cReturnType
        [ (Just pName, pType) | Parameter {..} <- V.toList cParameters ]
      )
  apps <-
    fmap (indent 2 . appList)
    . forV deviceCommands
    $ \MarshaledCommand { mcCommand = Command {..} } -> do
        fTyDoc <- renderTypeHighPrecSource =<< cToHsType
          DoLower
          (C.Proto
            cReturnType
            [ (Just pName, pType) | Parameter {..} <- V.toList cParameters ]
          )
        tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
        pure
          $ parens
              [qqi|castFunPtr @_ @{fTyDoc} <$> getDeviceProcAddr' handle (Ptr "{cName}\\NUL"#)|]
  tellExport (ETerm n)
  tellDoc [qqi|
    {n} :: {tDoc}
    {n} instanceCmds handle = do
      pGetDeviceProcAddr <- castFunPtr @_ @{getDeviceProcAddrTDoc}
          <$> vkGetInstanceProcAddr' (instanceCmdsHandle instanceCmds) (GHC.Ptr.Ptr "vkGetDeviceProcAddr\\NUL"#)
      let getDeviceProcAddr' = mkVkGetDeviceProcAddr pGetDeviceProcAddr
      DeviceCmds handle
        {apps}
  |]

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
  RenderParams {..} <- ask
  Command {..}      <-
    maybe (throw "Unable to find vkGetInstanceProcAddr command") pure
      =<< getCommand "vkGetInstanceProcAddr"
  ty <- cToHsType
    DoLower
    (C.Proto cReturnType
             [ (Just pName, pType) | Parameter {..} <- V.toList cParameters ]
    )
  tDoc <- renderTypeSource ty
  let n = mkFunName "vkGetInstanceProcAddr'"
  tellExport (ETerm n)
  tellDoc [qqi|
    -- | A version of 'vkGetInstanceProcAddr' which can be called with a
    -- null pointer for the instance.
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
  RenderParams {..} <- ask
  Command {..}      <-
    maybe (throw "Unable to find vkGetDeviceProcAddr command") pure
      =<< getCommand "vkGetDeviceProcAddr"
  ty <- cToHsType
    DoLower
    (C.Proto cReturnType
             [ (Just pName, pType) | Parameter {..} <- V.toList cParameters ]
    )
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

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
